module Main where
import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core
import Data.IORef

newtype Calculator = C (Calc, Double)

data Calc = Num Double | Sign Calc | Add Calc Calc | Min Calc Calc |
            Div Calc Calc | Mul Calc Calc

instance Show Calc where
    show (Num i)   = if floor i == ceiling i then show (floor i) else show i -- Hack to stop showing the .0 on whole numbers
    show (Sign c)  = "±(" ++ show c ++ ")"
    show (Add c d) = show c ++ " + " ++ show d
    show (Min c d) = show c ++ " - " ++ show d
    show (Div c d) = show c ++ " ÷ " ++ show d
    show (Mul c d) = show c ++ " x " ++ show d

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
    buttonsOps <- mapM createButton ["+", "-", "x", "÷"]
    buttonPi <- createButton "π"
    buttonE <- createButton "e"
    buttonRt2 <- createButton "√2"
    buttonClr  <- createButton "C"
    buttonCrEnt <- createButton "CE"
    buttonSign <- createButton "±"
    buttonAns  <- createButton "Ans"
    buttonEq   <- createButton "="

    let eventsNum = zipWith createActionInt buttonsNum [0..9]
    let eventsOps = zipWith createAction buttonsOps [(Add (Num 1) (Num 1)), (Min (Num 1) (Num 1)), (Mul (Num 1) (Num 1)), (Div (Num 1) (Num 1))]
    let eventPi    = (\x -> addPiCalculator x) <$ UI.click buttonPi
    let eventE     = (\x -> addECalculator x) <$ UI.click buttonE
    let eventRt2   = (\x -> addRt2Calculator x) <$ UI.click buttonRt2
    let eventClear = (const (C ((Num 0), 0))) <$ UI.click buttonClr
    let eventCrEnt = (\x -> clearEntry x) <$ UI.click buttonCrEnt
    let eventSign  = (\x -> addSign x) <$ UI.click buttonSign
    let eventAns   = (\x -> addAnswerCalculator x) <$ UI.click buttonAns
    let eventEqual = (\x -> evaluateCalc x) <$ UI.click buttonEq

    let events = eventsNum ++ eventsOps ++ [eventClear, eventSign, eventEqual, eventCrEnt, eventAns, eventPi, eventE, eventRt2]

    calculator <- accumB (C ((Num 0), 0)) $ foldl1 (unionWith const) events
    ansDisplay   <- UI.label # sink UI.text (fmap showAns calculator)
    calcDisplay   <- UI.label # sink UI.text (fmap showCalc calculator)

    -- Wanna change this so that it looks better
    -- Try and get the numbers in a relevant order
    getBody window #+ [UI.center #+ ([element ansDisplay, UI.br, element calcDisplay, UI.br]++
                                     [ element b | b <- buttonsNum ] ++ [UI.br] ++
                                     [ element b | b <- buttonsOps ] ++ [UI.br] ++
                                     [element buttonCrEnt, element buttonClr, element buttonSign, element buttonAns, UI.br] ++
                                     [element buttonPi, element buttonE, element buttonRt2, UI.br] ++
                                     [element buttonEq]
                                     )]
    return ()

addDigitCalculator :: Calculator -> Double -> Calculator
addDigitCalculator (C (c, d)) i = (C ((addDigitCalc c i), d))

addDigitCalc :: Calc -> Double -> Calc
addDigitCalc (Num x)   i = Num (x*10 + i)
addDigitCalc (Sign c)  i = Sign (addDigitCalc c i)
addDigitCalc (Add d c) i = Add d (addDigitCalc c i)
addDigitCalc (Min d c) i = Min d (addDigitCalc c i)
addDigitCalc (Div d c) i = Div d (addDigitCalc c i)
addDigitCalc (Mul d c) i = Mul d (addDigitCalc c i)

addSign :: Calculator -> Calculator
addSign (C ((Sign c), d)) = (C (c, d))
addSign (C (c, d)) = (C ((Sign c), d))

createButton :: String -> UI Element
createButton s = UI.button # set UI.text s

createAction :: Element -> Calc -> Event (Calculator -> Calculator)
createAction button (Add _ _) = (\(C (c, d)) -> (C ((Add c (Num 0)), d)) ) <$ UI.click button
createAction button (Min _ _) = (\(C (c, d)) -> (C ((Min c (Num 0)), d)) ) <$ UI.click button
createAction button (Mul _ _) = (\(C (c, d)) -> (C ((Mul c (Num 0)), d)) ) <$ UI.click button
createAction button (Div _ _) = (\(C (c, d)) -> (C ((Div c (Num 0)), d)) ) <$ UI.click button

-- Operations are evaluated left to right NOT in order of precedence
evaluateCalc :: Calculator -> Calculator
evaluateCalc (C (c, d)) = (C ((Num 0), evaluate c))

evaluate :: Calc -> Double
evaluate (Num i)   = i
evaluate (Add c d) = (evaluate c) + (evaluate d)
evaluate (Mul c d) = (evaluate c) * (evaluate d)
evaluate (Div c d) = (evaluate c) / (evaluate d)
evaluate (Min c d) = (evaluate c) - (evaluate d)
evaluate (Sign c)  = (-1) * (evaluate c)

createActionInt :: Element -> Double -> Event (Calculator -> Calculator)
createActionInt button i = (\x -> addDigitCalculator x i) <$ UI.click button

clearEntry :: Calculator -> Calculator
clearEntry (C (c, d)) = (C (c, 0))

addAnswerCalculator :: Calculator -> Calculator
addAnswerCalculator (C (c, d)) = (C ((changeLastNumCalc c d), d))

addPiCalculator :: Calculator -> Calculator
addPiCalculator (C (c, d)) = (C ((changeLastNumCalc c pi), d))

addECalculator :: Calculator -> Calculator
addECalculator (C (c, d)) = (C ((changeLastNumCalc c (exp 1)), d))

addRt2Calculator :: Calculator -> Calculator
addRt2Calculator (C (c, d)) = (C ((changeLastNumCalc c (sqrt 2)), d))

changeLastNumCalc :: Calc -> Double -> Calc
changeLastNumCalc (Num x)   a = Num a
changeLastNumCalc (Sign c)  a = Sign  (changeLastNumCalc c a)
changeLastNumCalc (Add d c) a = Add d (changeLastNumCalc c a)
changeLastNumCalc (Min d c) a = Min d (changeLastNumCalc c a)
changeLastNumCalc (Div d c) a = Div d (changeLastNumCalc c a)
changeLastNumCalc (Mul d c) a = Mul d (changeLastNumCalc c a)

showAns :: Calculator -> String
showAns (C (_, d)) = "Ans: " ++ (if floor d == ceiling d then show (floor d) else show d)

showCalc :: Calculator -> String
showCalc (C (c, _)) = show c
