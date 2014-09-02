#!/usr/bin/env python
#
# zowie.py -- Interpreter for the ZOWIE language
# Chris Pressey, Cat's Eye Technologies, Oct 6 2009
# Adapted to run under Skulpt Oct 10 2013
# This source code is in the public domain.
#

import re
import sys


# check if running under Skulpt, and if so, apply appropriate modifications
if getattr(sys, 'resetTimeout', None) is not None:
    __name__ = '__skulpt__'

    class EOFError:
        pass

    class SyntaxError:
        pass

    class UnicodeEncodeError:
        pass

    def output(code):
        print "&#%d;" % code
else:
    def output(code):
        try:
            sys.stdout.write(unichr(code))
        except UnicodeEncodeError:
            sys.stdout.write("&#%d;" % code)


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
        try:
            c = sys.stdin.read(1)
            if len(c) == 0:
                x = 0
            else:
                x = ord(c)
        except EOFError:
            x = 0
        return x

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
        self.state[8] = self.state[8] + payload


class SubtractionRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 5

    def write(self, payload):
        result = self.state[8] - payload
        if result < 0:
            result = 0
        self.state[8] = result


class MultiplicationRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 6

    def write(self, payload):
        self.state[8] = self.state[8] * payload


class NegationRegister(MappedRegister):
    def __init__(self, state):
        self.state = state

    def read(self):
        return 7

    def write(self, payload):
        if payload == 0:
            self.state[8] = 1
        else:
            self.state[8] = 0


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
        if key in self.mmap_register:
            return self.mmap_register[key].read()
        if key not in self.storage_register:
            self.storage_register[key] = 0
        return self.storage_register[key]

    def __setitem__(self, key, value):
        if key in self.mmap_register:
            self.mmap_register[key].write(value)
        self.storage_register[key] = value


class Reference(object):  # abstract
    pass


class ImmediateReference(Reference):
    def __init__(self, number):
        self.number = number


class DirectRegisterReference(Reference):
    def __init__(self, number):
        self.number = number


class IndirectRegisterReference(Reference):
    def __init__(self, regref):
        self.regref = regref


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
        return number

    def scan_reference(self):
        if self.line[0] == 'R':
            self.line = self.line[1:]
            if self.line[0] == '[':
                self.line = self.line[1:]
                inner = self.scan_register()
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
        self.source_register = 0
        self.destination_register = 0
        self.source_is_immediate = False
        self.source_is_indirect = False
        self.destination_is_indirect = False

    def parse(self, line):
        line = line.strip()
        if not line or line[0] == ';':
            return False

        scanner = Scanner(line)
        scanner.expect('MOV')
        line = scanner.line

        # lhs = scanner.scan_reference()

        m = re.match(r'^R(\d+)\s*,\s*(\d+)\s*(\;.*)?$', line)
        if m is not None:
            self.source_is_immediate = True
            dest_reg = m.group(1)
            src_imm = m.group(2)
            self.destination_register = long(dest_reg)
            self.source_register = long(src_imm)
            return True

        # We actually implement a syntactic superset of ZOWIE here -- the
        # closing bracket is just sugar which may be omitted or included
        # without changing the meaning (only the opening bracket counts!)
        m = re.match(r'^R(\[R)?(\d+)\]?\s*,\s*R(\[R)?(\d+)\]?'
                     r'\s*(\;.*)?$', line)
        if m is not None:
            dest_ind = m.group(1)
            dest_reg = m.group(2)
            src_ind = m.group(3)
            src_reg = m.group(4)
            if dest_ind == '[R':
                self.destination_is_indirect = True
            self.destination_register = long(dest_reg)
            if src_ind == '[R':
                self.source_is_indirect = True
            self.source_register = long(src_reg)
            return True

        raise SyntaxError("Could not parse line '%s'" % line)

    def apply(self, state):
        if self.source_is_indirect:
            source_register = state[self.source_register]
        else:
            source_register = self.source_register

        if self.source_is_immediate:
            contents = source_register
        else:
            contents = state[source_register]

        if self.destination_is_indirect:
            destination_register = state[self.destination_register]
        else:
            destination_register = self.destination_register
        state[destination_register] = contents

    def __str__(self):
        dest_fmt = "R%d"
        if self.destination_is_indirect:
            dest_fmt = "R[R%d]"
        src_fmt = "R%d"
        if self.source_is_indirect:
            src_fmt = "R[R%d]"
        if self.source_is_immediate:
            src_fmt = "%d"
        return "MOV %s, %s" % (dest_fmt % self.destination_register,
                               src_fmt % self.source_register)


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


def rpython_main(argv):
    # EXPERIMENTAL!
    print "hi there"
    p = Processor()
    p.load_string("""
MOV R10, 90   ; initially it's "Z"
MOV R1, R1    ; BEGIN TRANSACTION for "REPEAT"
MOV R0, R10   ; output character
MOV R8, R10   ; decrement character
MOV R5, 1
MOV R10, R8
MOV R8, R10   ; test if character is above "@"
MOV R5, 64
MOV R3, R8    ; COMMIT AND REPEAT if non-zero
""")
    p.run()
    return 0


def target(*args):
    return rpython_main, None


if __name__ == "__main__":
    main(sys.argv)
