## TODOS (remove after completion)

- Create diagrams showing transactions
- Remove `Use` Redeemer from Oracle and use it as reference input + inline datum
- Create off chain code
- Change `Minting.hs` code to be more concise
- Change `Collateral.hs` code to be more concise
- Write tests (already started)

## How the algorithm works

```mermaid
flowchart LR
    subgraph Deploy [Deploy Oracle]
    direction LR
    A1((User 1)) --> A2(Mint NFT)
    A2 --> A3((User 1 <br> <sub>NFT</sub>))
    A3 -->|<sub>NFT <br> + <br> ADA/USD Rate</sub>| A4(Deploy Oracle)
    A4 --> A5((User 1))
    A4 --> A6(("Oracle Validator <br> <sub>(NFT with ADA/USD rate in Datum)</sub>"))
    end
    subgraph Mint [Mint Stablecoin]
    direction LR
    M1((User 2)) --> |Locks ADA as collateral| M4
    M2((Minting Policy)) --> |As reference script?| M4
    M3((Collateral Val)) --> |As reference script?| M4
    direction LR
    M4(Lock Collateral <br> + <br> Mint stablecoin)
    A6 --> |Provides rate as reference Input|M4
    M4 --> M5(("<b>Collateral Val</b> (Datum has:) <br> <sub>Collateral owner</sub>  <br> <sub>Amount of stablecoin minted</sub>"))
    M4 --> M6((User 2 <br> <sub>Stablecoins</sub>))
    end

  

```