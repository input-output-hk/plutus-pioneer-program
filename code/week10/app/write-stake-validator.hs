import System.Environment (getArgs)
import Text.Printf        (printf)
import Week10.Deploy      (tryReadAddress, writeStakeValidator)

main :: IO ()
main = do
    [file, addr'] <- getArgs
    let Just addr = tryReadAddress addr'
    printf "file: %s\naddr: %s\n" file (show addr)
    e <- writeStakeValidator file addr
    case e of
        Left err -> print err
        Right () -> printf "wrote stake validator to %s\n" file
