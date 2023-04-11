{-# LANGUAGE OverloadedStrings #-}

import Language.Marlowe.Extended.V1 hiding (contract)

main :: IO ()
main = printJSON $ contract "Charles" "Simon" "Alex" $ Constant 100

choiceId :: Party -> ChoiceId
choiceId = ChoiceId "Winner"

contract :: Party -> Party -> Party -> Value -> Contract
contract alice bob charlie deposit =
    When
        [ f alice bob
        , f bob alice
        ]
        10 Close
  where
    f :: Party -> Party -> Case
    f x y =
        Case
            (Deposit
                x
                x
                ada
                deposit
            )
            (When
                [Case
                    (Deposit
                        y
                        y
                        ada
                        deposit
                    )
                    (When
                        [Case
                            (Choice
                                (choiceId charlie)
                                [Bound 1 2]
                            )
                            (If
                                (ValueEQ
                                    (ChoiceValue $ choiceId charlie)
                                    (Constant 1)
                                )
                                (Pay
                                    bob
                                    (Account alice)
                                    ada
                                    deposit
                                    Close
                                )
                                (Pay
                                    alice
                                    (Account bob)
                                    ada
                                    deposit
                                    Close
                                )
                            )]
                        30 Close
                    )]
                20 Close
            )