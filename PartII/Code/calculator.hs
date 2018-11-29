module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import Data.IORef

data Calc = Num Double | Sign Calc | Add Calc Calc | Min Calc Calc |
            Div Calc Calc | Mul Calc Calc | Calc 'Mul' Calc

instance Show Calc where
    show (Num i)   = if floor i == ceiling i then show (floor i) else show i -- Hack to stop showing the .0 on whole numbers
    show (Sign c)  = "±(" ++ show c ++ ")"
    show (Add c d) = show c ++ " + " ++ show d
    show (Min c d) = show c ++ " - " ++ show d
    show (Div c d) = show c ++ " ÷ " ++ show d
    show (Mul c d) = show c ++ " * " ++ show d

main :: IO ()
main = do
    startGUI defaultConfig
        { jsPort       = Just 8023
        , jsStatic     = Just "../wwwroot"
        } setup

setup :: Window -> UI ()
setup window = do
    return window # set UI.title "G54RFP Calculator"

    -- Create all the buttons from strings
    buttonsNum <- mapM createButton (map show [0..9])
    buttonsOps <- mapM createButton ["+", "-", "*", "÷"]
    buttonClr  <- createButton "C"
    buttonCrEnt <- createButton "CE"
    buttonSign <- createButton "±"
    buttonEq   <- createButton "="

    let eventsNum = zipWith createActionInt buttonsNum [0..9]
    let eventsOps = zipWith createAction buttonsOps ["+", "-", "*", "÷"]
    let eventClear = (const (Num 0)) <$ UI.click buttonClr
    let eventCrEnt = (\x -> clearEntry x) <$ UI.click buttonCrEnt
    let eventSign  = (\x -> addSign x) <$ UI.click buttonSign
    let eventEqual = (\x -> evaluateCalc x) <$ UI.click buttonEq

    let events = eventsNum ++ eventsOps ++ [eventClear, eventSign, eventEqual, eventCrEnt]

    -- let events = eventsNum ++ eventsOps ++ [eventClear, eventPlMi]

    number <- accumB (Num 0) $ foldl1 (unionWith const) events
    calcDisplay   <- UI.label # sink UI.text (fmap show number)

    -- Wanna change this so that it looks better
    -- Try and get the numbers in a relevant order
    getBody window #+ [UI.center #+ ([element calcDisplay, UI.br]++

                                     [ element b | b <- buttonsNum ] ++ [UI.br] ++

                                     [ element b | b <- buttonsOps ] ++ [UI.br] ++
                                     [element buttonCrEnt, element buttonClr, element buttonSign, UI.br] ++
                                     [element buttonEq]
                                     )]
    return ()

addDigitCalc :: Calc -> Double -> Calc
addDigitCalc (Num x)   i = Num (x*10 + i)
addDigitCalc (Sign c)  i = Sign (addDigitCalc c i)
addDigitCalc (Add d c) i = Add d (addDigitCalc c i)
addDigitCalc (Min d c) i = Min d (addDigitCalc c i)
addDigitCalc (Div d c) i = Div d (addDigitCalc c i)
addDigitCalc (Mul d c) i = Mul d (addDigitCalc c i)

addSign :: Calc -> Calc
addSign (Sign c) = c
addSign c = Sign c

createButton :: String -> UI Element
createButton s = UI.button # set UI.text s

createAction :: Element -> String -> Event (Calc -> Calc)
createAction button s | s == "+" = (\x -> (Add x (Num 0))) <$ UI.click button
createAction button s | s == "-" = (\x -> (Min x (Num 0))) <$ UI.click button
createAction button s | s == "*" = (\x -> (Mul x (Num 0))) <$ UI.click button
createAction button s | s == "÷" = (\x -> (Div x (Num 0))) <$ UI.click button

-- Operations are evaluated left to right NOT in order of precedence
evaluateCalc :: Calc -> Calc
evaluateCalc c = Num (evaluate c)

-- evaluateCalc :: Calc -> Calc
-- evaluateCalc c = Num (evaluateMul c)
--
-- rightMostNum :: Calc -> Double
-- rightMostNum (Num i) = i
-- rightMostNum (Sign c) = rightMostNum c
-- rightMostNum (Add c (Num i)) = i
-- rightMostNum (Add c d) = rightMostNum d
-- rightMostNum (Min c d) = rightMostNum d
-- rightMostNum (Mul c d) = rightMostNum d
-- rightMostNum (Div c d) = rightMostNum d
--
-- evaluateMul :: Calc -> Double
-- evaluateMul (Num i)   = i
-- evaluateMul (Mul c d) = (rightMostNum c) * (evaluateMul d)

evaluate :: Calc -> Double
evaluate (Num i)   = i
evaluate (Add c d) = (evaluate c) + (evaluate d)
evaluate (Mul c d) = (evaluate c) * (evaluate d)
evaluate (Div c d) = (evaluate c) / (evaluate d)
evaluate (Min c d) = (evaluate c) - (evaluate d)
evaluate (Sign c)  = (-1) * (evaluate c)

createActionInt :: Element -> Double -> Event (Calc -> Calc)
createActionInt button i = (\x -> addDigitCalc x i) <$ UI.click button

clearEntry :: Calc -> Calc
clearEntry (Num x)   = Num 0
clearEntry (Sign c)  = Sign (clearEntry c)
clearEntry (Add d c) = Add d (clearEntry c)
clearEntry (Min d c) = Min d (clearEntry c)
clearEntry (Div d c) = Div d (clearEntry c)
clearEntry (Mul d c) = Mul d (clearEntry c)
