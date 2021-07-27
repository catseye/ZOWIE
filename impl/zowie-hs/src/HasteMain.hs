module Main where

import Haste.DOM (withElems, getValue, setProp)
import Haste.Events (onEvent, MouseEvent(Click))

import qualified Language.ZOWIE.Parser as Parser
import qualified Language.ZOWIE.Machine as Machine


main = withElems ["prog", "result", "run-button"] driver

driver [progElem, resultElem, runButtonElem] =
    onEvent runButtonElem Click $ \_ -> do
        Just text <- getValue progElem
        case Parser.parseZOWIE text of
            Right prog -> do
                result <- Machine.loadAndRun prog
                setProp resultElem "textContent" $ show result
            Left error ->
                setProp resultElem "textContent" $  show error
