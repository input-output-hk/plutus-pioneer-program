Troubleshooting
===============

Error codes
-----------

To reduce code size, on-chain errors only output codes. Here's what they mean:

..
  This list can be generated with:
  grep -rEoh "\btrace\w*\s+\"[^\"]{1,5}\"\s+(--.*|\{-\".*\"-\})" *

- Ledger errors

  - ``L0: Input constraint``
  - ``L1: Output constraint``
  - ``L2: Missing datum``
  - ``L3: Wrong validation interval``
  - ``L4: Missing signature``
  - ``L5: Spent value not OK``
  - ``L6: Produced value not OK``
  - ``L7: Public key output not spent``
  - ``L8: Script output not spent``
  - ``L9: Value minted not OK``
  - ``La: MustPayToPubKey``
  - ``Lb: MustPayToOtherScript``
  - ``Lc: MustHashDatum``
  - ``Ld: checkScriptContext failed``
  - ``Le: Can't find any continuing outputs``
  - ``Lf: Can't get any continuing outputs``
  - ``Lg: Can't get validator and datum hashes``
  - ``Lh: Can't get currency symbol of the current validator script``
  - ``Li: DecodingError``

- State machine errors

  - ``S0: Can't find validation input``
  - ``S1: State transition invalid - checks failed``
  - ``S2: Thread token not found``
  - ``S3: Non-zero value allocated in final state``
  - ``S4: State transition invalid - constraints not satisfied by ScriptContext``
  - ``S5: State transition invalid - constraints not satisfied by ScriptContext``
  - ``S6: State transition invalid - input is not a valid transition at the current state``
  - ``S7: Value minted different from expected``
  - ``S8: Pending transaction does not spend the designated transaction output``

- Currency errors

  - ``C0: Value minted different from expected``
  - ``C1: Pending transaction does not spend the designated transaction output``
