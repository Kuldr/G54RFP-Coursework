--Dining Philosophers-----------------------------------------------------------
-- Helper Functions
ustos :: Int -> Int
ustos i = i * 1000000

getTimeString :: IO String
getTimeString = do
                    zt <- getZonedTime
                    return $ formatTime defaultTimeLocale "%H:%M:%S" zt

printPhilosopherIntent :: String -> String -> IO ()
printPhilosopherIntent name i =
    do
        timeString <- getTimeString
        putStrLn (timeString ++ " | " ++ name ++ " is " ++ i)

delayPhilosopherPrint :: String -> Int -> IO ()
delayPhilosopherPrint name s =
    do
        timeString <- getTimeString
        putStrLn (timeString ++ " | Delaying " ++ name ++
                    " " ++ show(s) ++ " seconds")
        threadDelay (ustos s)

-- Philosopher Function
philosopher :: String -> ((TMVar Int), (TMVar Int)) -> IO ()
philosopher name (ls,rs) =
    do
    -- Work happens here
    philosopher name (ls,rs)

printPhilosopherIntent name "thinking"
seconds <- randomRIO (1, 10)
delayPhilosopherPrint name seconds

printPhilosopherIntent name "hungry"
(l, r) <- atomically $ do
    l <- takeTMVar ls
    r <- takeTMVar rs
    return (l, r)

printPhilosopherIntent name "eating"
seconds <- randomRIO (1, 10)
delayPhilosopherPrint name seconds
atomically $ do
    putTMVar ls l
    putTMVar rs r

-- Main Function
main = do
    sporks <- mapM newTMVarIO [1..length(philosophers)]
    let pairs = zip sporks (tail(sporks) ++ [(head sporks)])

    mapM forkIO $ zipWith philosopher philosophers pairs

    forever $ threadDelay (ustos 1)
--Electronic Calculator---------------------------------------------------------
-- Digit
addDigitCalculator :: Calculator -> Double -> Calculator
addDigitCalculator (C (c, d)) i = (C ((addDigitCalc c i), d))

addDigitCalc :: Calc -> Double -> Calc
addDigitCalc (Num x)   i = Num (x*10 + i)
addDigitCalc (Sign c)  i = Sign (addDigitCalc c i)
addDigitCalc (Add d c) i = Add d (addDigitCalc c i)
-- Same definition as Add for other operators

-- Reset and Clear
clearEntry :: Calculator -> Calculator
clearEntry (C (c, _)) = (C (c, 0))

clearCalculator :: Calculator
clearCalculator = C ((Num 0), 0)

-- Evaluation
evaluateCalc :: Calculator -> Calculator
evaluateCalc (C (c, _)) = (C ((Num 0), evaluate c))

evaluate :: Calc -> Double
evaluate (Num i)   = i
evaluate (Sign c)  = (-1) * (evaluate c)
evaluate (Add c d) = (evaluate c) + (evaluate d)
-- Same definition as Add for other operators

-- Add Answer
addAnswerCalculator :: Calculator -> Calculator
addAnswerCalculator (C (c, d)) = (C ((changeLastNumCalc c d), d))

changeLastNumCalc :: Calc -> Double -> Calc
changeLastNumCalc (Num x)   a = Num a
changeLastNumCalc (Sign c)  a = Sign  (changeLastNumCalc c a)
changeLastNumCalc (Add d c) a = Add d (changeLastNumCalc c a)
-- Same definition as Add for other operators

-- Constants
addPiCalculator, addECalculator, addRt2Calculator :: Calculator -> Calculator
addPiCalculator  (C (c, d)) = (C ((changeLastNumCalc c pi),       d))
addECalculator   (C (c, d)) = (C ((changeLastNumCalc c (exp 1)),  d))
addRt2Calculator (C (c, d)) = (C ((changeLastNumCalc c (sqrt 2)), d))

-- Sign Change
changeSign :: Calculator -> Calculator
changeSign (C ((Sign c), d)) = (C (c, d))
changeSign (C (c, d)) = (C ((Sign c), d))

-- Extra Code
-- Button Creation
createButton :: String -> UI Element
createButton s = UI.button # set UI.text s

-- Show Calculator
showAns :: Calculator -> String
showAns (C (_, d)) = "Ans: " ++ (if floor d == ceiling d
                                 then show (floor d) else show d)

showCalc :: Calculator -> String
showCalc (C (c, _)) = show c

instance Show Calc where
    show (Num i)   = if floor i == ceiling i then show (floor i) else show i
    show (Sign c)  = "±(" ++ show c ++ ")"
    show (Add c d) = show c ++ " + " ++ show d
    -- Same definition as Add for other operators

-- Creating common actions
createActionInt :: Element -> Double -> Event (Calculator -> Calculator)
createActionInt button i = (\x -> addDigitCalculator x i) <$ UI.click button

createAction :: Element -> Calc -> Event (Calculator -> Calculator)
createAction button (Add _ _) = ( \(C (c, d)) -> (C ((Add c (Num 0)), d)) ) <$ UI.click button
-- Same definition as Add for other operators

-- Setup
-- Main idea
setup :: Window -> UI ()
setup window = do
    return window # set UI.title "G54RFP Calculator"

-- Create all the buttons from strings
buttonsNum  <- mapM createButton (map show [0..9])
buttonsOps  <- mapM createButton ["+", "-", "x", "/"]
buttonPi    <- createButton "pi"
buttonE     <- createButton "e"
buttonRt2   <- createButton "sqrt2"
buttonClr   <- createButton "C"
buttonCrEnt <- createButton "CE"
buttonSign  <- createButton "+/-"
buttonAns   <- createButton "Ans"
buttonEq    <- createButton "="

-- Create all the events by adding functions to the associated buttons
let eventsNum  = zipWith createActionInt buttonsNum [0..9]
let eventsOps  = zipWith createAction buttonsOps [(Add (Num 1) (Num 1)),
                                                  (Min (Num 1) (Num 1)),
                                                  (Mul (Num 1) (Num 1)),
                                                  (Div (Num 1) (Num 1))]
let eventPi    = (\x -> addPiCalculator x) <$ UI.click buttonPi
let eventE     = (\x -> addECalculator x) <$ UI.click buttonE
let eventRt2   = (\x -> addRt2Calculator x) <$ UI.click buttonRt2
let eventClear = (const clearCalculator) <$ UI.click buttonClr
let eventCrEnt = (\x -> clearEntry x) <$ UI.click buttonCrEnt
let eventSign  = (\x -> changeSign x) <$ UI.click buttonSign
let eventAns   = (\x -> addAnswerCalculator x) <$ UI.click buttonAns
let eventEqual = (\x -> evaluateCalc x) <$ UI.click buttonEq

let events = eventsNum ++ eventsOps ++ [eventClear, eventSign, eventEqual, eventCrEnt, eventAns, eventPi, eventE, eventRt2]

-- Add the event handler to access the calculator with a starting value of reset
calculator  <- accumB clearCalculator $ foldl1 (unionWith const) events

-- Make labels with the current calculation and previous answer
ansDisplay  <- UI.label # sink UI.text (fmap showAns calculator)
calcDisplay <- UI.label # sink UI.text (fmap showCalc calculator)

-- Add all of the buttons to window
getBody window #+
    [UI.center #+
        ([ element ansDisplay, UI.br, element calcDisplay, UI.br]++
         [ element b | b <- buttonsNum ] ++ [UI.br] ++
         [ element b | b <- buttonsOps ] ++ [UI.br] ++
         [ element buttonCrEnt, element buttonClr, element buttonSign,
                element buttonAns, UI.br] ++
         [ element buttonPi, element buttonE, element buttonRt2, UI.br] ++
         [ element buttonEq]
         )]
return ()
