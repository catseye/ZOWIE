#!/usr/bin/env python
#
# zowie.py -- Interpreter for the ZOWIE language
# Chris Pressey, Cat's Eye Technologies, Oct 6 2009
# Adapted to run under Skulpt Oct 10 2013
# Adapted to compile under RPython Sep 2014
# This source code is in the public domain.
#

import sys


def input():
    try:
        c = sys.stdin.read(1)
        if len(c) == 0:
            x = 0
        else:
            x = ord(c)
    except EOFError:
        x = 0
    return x


def unichr_compat(code):
    try:
        return unichr(code)
    except NameError:
        return chr(code)


def output(code):
    try:
        sys.stdout.write(unichr_compat(code))
    except UnicodeEncodeError:
        sys.stdout.write("&#%d;" % code)


# check if running under Skulpt, and if so, apply appropriate modifications
if getattr(sys, 'resetTimeout', None) is not None:
    __name__ = '__skulpt__'

    class EOFError:
        pass

    class SyntaxError:
        pass

    class UnicodeEncodeError:
        pass

    def skulpt_output(code):
        print("&#%d;" % code)

    output = skulpt_output


def copy_dict(d):
    e = {}
    for key in d:
        e[key] = d[key]
    return e


class MappedRegister(object):  # abstract
    def read(self):
        raise NotImplementedError

    def write(self, payload):
        raise NotImplementedError


class TtyRegister(MappedRegister):
    def __init__(self, cpu):
        pass

    def read(self):
        return input()

    def write(self, payload):
        output(payload)


class BeginTransactionRegister(MappedRegister):
    def __init__(self, cpu):
        self.cpu = cpu

    def read(self):
        return 1

    def write(self, payload):
        self.cpu.begin_transaction()


class CommitRegister(MappedRegister):
    def __init__(self, cpu):
        self.cpu = cpu

    def read(self):
        return 2

    def write(self, payload):
        if payload > 0:
            self.cpu.commit()
        else:
            self.cpu.rollback()


class CommitAndRepeatRegister(MappedRegister):
    def __init__(self, cpu):
        self.cpu = cpu

    def read(self):
        return 3

    def write(self, payload):
        if payload > 0:
            self.cpu.commit_and_repeat()
        else:
            self.cpu.commit()


class AdditionRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 4

    def write(self, payload):
        self.state.__setitem__(8, self.state.__getitem__(8) + payload)


class SubtractionRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 5

    def write(self, payload):
        result = self.state.__getitem__(8) - payload
        if result < 0:
            result = 0
        self.state.__setitem__(8, result)


class MultiplicationRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 6

    def write(self, payload):
        self.state.__setitem__(8, self.state.__getitem__(8) * payload)


class NegationRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 7

    def write(self, payload):
        if payload == 0:
            self.state.__setitem__(8, 1)
        else:
            self.state.__setitem__(8, 0)


class MachineState(object):
    def __init__(self, cpu):
        self.cpu = cpu
        self.pc = 0
        self.storage_register = {}
        self.mmap_register = {}
        self.mmap_register[0] = TtyRegister(cpu)
        self.mmap_register[1] = BeginTransactionRegister(cpu)
        self.mmap_register[2] = CommitRegister(cpu)
        self.mmap_register[3] = CommitAndRepeatRegister(cpu)
        self.mmap_register[4] = AdditionRegister(self)
        self.mmap_register[5] = SubtractionRegister(self)
        self.mmap_register[6] = MultiplicationRegister(self)
        self.mmap_register[7] = NegationRegister(self)

    def clone(self):
        other = MachineState(self.cpu)
        other.pc = self.pc
        other.storage_register = copy_dict(self.storage_register)
        return other

    def __getitem__(self, key):
        assert isinstance(key, int)
        if key in self.mmap_register:
            return self.mmap_register[key].read()
        if key not in self.storage_register:
            self.storage_register[key] = 0
        return self.storage_register[key]

    def __setitem__(self, key, value):
        assert isinstance(key, int)
        assert isinstance(value, int)
        if key in self.mmap_register:
            self.mmap_register[key].write(value)
        self.storage_register[key] = value


class Reference(object):  # abstract
    def get_value(self, state):
        raise NotImplementedError

    def set_value(self, state, value):
        raise NotImplementedError


class ImmediateReference(Reference):
    def __init__(self, value):
        self.value = value

    def get_value(self, state):
        return self.value

    def __str__(self):
        return str(self.number)


class DirectRegisterReference(Reference):
    def __init__(self, index):
        self.index = index

    def get_value(self, state):
        return state.__getitem__(self.index)

    def set_value(self, state, value):
        state.__setitem__(self.index, value)

    def __str__(self):
        return "R%d" % self.number


class IndirectRegisterReference(Reference):
    def __init__(self, ref):
        assert isinstance(ref, Reference)
        self.ref = ref

    def get_value(self, state):
        return state.__getitem__(self.ref.get_value(state))

    def set_value(self, state, value):
        state.__setitem__(self.ref.get_value(state), value)

    def __str__(self):
        return "R[%s]" % self.ref


class Scanner(object):
    def __init__(self, line):
        self.line = line

    def expect(self, s):
        if not self.line or not self.line.startswith(s):
            raise SyntaxError("Expected '%s'" % s)
        self.line = self.line[len(s):].lstrip()

    def scan_integer(self):
        number = 0
        if not self.line or not self.line[0].isdigit():
            raise SyntaxError('Expected integer')
        while self.line and self.line[0].isdigit():
            number = number * 10 + (ord(self.line[0]) - ord('0'))
            self.line = self.line[1:]
        self.line = self.line.lstrip()
        return number

    def scan_reference(self):
        if self.line[0] == 'R':
            self.line = self.line[1:]
            if self.line[0] == '[':
                self.line = self.line[1:]
                inner = self.scan_reference()
                self.expect(']')
                return IndirectRegisterReference(inner)
            else:
                regnum = self.scan_integer()
                return DirectRegisterReference(regnum)
        else:
            imm = self.scan_integer()
            return ImmediateReference(imm)


class Instruction(object):
    def __init__(self):
        self.source_register = None
        self.destination_register = None

    def parse(self, line):
        line = line.strip()
        if not line or line[0] == ';':
            return False

        scanner = Scanner(line)
        scanner.expect('MOV')

        self.destination_register = scanner.scan_reference()

        assert not isinstance(self.destination_register, ImmediateReference)

        scanner.expect(',')
        self.source_register = scanner.scan_reference()

        if scanner.line and scanner.line[0] != ';':
            raise SyntaxError("Expected EOL or comment")
        
        return True

    def apply(self, state):
        value = self.source_register.get_value(state)
        self.destination_register.set_value(state, value)

    def __str__(self):
        return "MOV %s, %s" % (self.destination_register, self.source_register)


class Processor(object):
    def __init__(self):
        self.program = []
        self.state = MachineState(self)
        self.states = []

    def __str__(self):
        s = ""
        for i in self.program:
            s += str(i) + "\n"
        return s

    def load(self, filename):
        f = open(filename, 'r')
        for line in f:
            i = Instruction()
            if i.parse(line):
                self.program.append(i)
        f.close()

    def load_string(self, text):
        for line in text.split("\n"):
            i = Instruction()
            if i.parse(line):
                self.program.append(i)

    def step(self):
        try:
            i = self.program[self.state.pc]
        except IndexError:
            return False

        i.apply(self.state)

        self.state.pc += 1
        return True

    def run(self):
        running = True
        while running:
            running = self.step()

    def begin_transaction(self):
        self.states.append(self.state.clone())

    def rollback(self):
        pc = self.state.pc
        self.state = self.states.pop()
        self.state.pc = pc

    def commit(self):
        self.states.pop()

    def commit_and_repeat(self):
        discard = self.states.pop()
        self.state.pc = discard.pc - 1


def main(argv):
    p = Processor()
    for filename in argv[1:]:
        p.load(filename)
        p.run()


def target(*args):
    global input, output

    import os

    def rpython_input():
        s = os.read(1, 1)
        if not s:
            return 0
        return ord(s[0])


    def rpython_output(code):
        if code <= 127:
            os.write(0, chr(code))
        else:
            os.write(0, "&#%d;" % code)


    def rpython_load(filename):
        fd = os.open(filename, os.O_RDONLY, 0o644)
        text = ''
        chunk = os.read(fd, 1024)
        text += chunk
        while len(chunk) == 1024:
            chunk = os.read(fd, 1024)
            text += chunk
        os.close(fd)
        return text


    def rpython_main(argv):
        p = Processor()
        program = rpython_load(argv[1])
        p.load_string(program)
        p.run()
        return 0

    input = rpython_input
    output = rpython_output
    return rpython_main, None


if __name__ == "__main__":
    main(sys.argv)
