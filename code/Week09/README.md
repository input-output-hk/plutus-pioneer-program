> Navigate to this file in GitHub or install the [Markdown Preview Mermaid Support](https://marketplace.visualstudio.com/items?itemName=bierner.markdown-mermaid) extension in VSCode to be able to see the diagrams.

> This implementation is for learning purposes only and has some issues introduced for the pioneers to solve as homework. It's not the only way to implement an over-collateralized stablecoin, and we don't recommend using this algorithm in production.

## How our stablecoin works

Depending on the stablecoin, you might see different mechanisms to (try to) maintain the peg. In this lecture, we’ll implement an over-collateralized algorithmic stablecoin that uses a liquidation mechanism to incentivize stability.

As the name suggests, we need to provide collateral to be able to mint the stablecoin. We will use a validator called "Collateral" to lock and release collateral (in ADA) and a minting policy called "Minting" to manage the minting and burning of stablecoins. These two will always run together—in a single transaction—as minting or burning stablecoins require locking or releasing collateral, respectively. Because we use smart contracts to enforce an algorithm that tries to control the price of the stablecoin, it's an algorithmic stablecoin.

The value of the collateral must exceed the value of the coins minted. This extra value that is locked but can't be minted is the reward someone gets when liquidating someone else's position. Liquidating means burning the same number of stablecoins someone else minted to get their collateral. You can liquidate someone else's position only if the relation between the value of their locked collateral and the coins they minted with it is below a certain pre-defined threshold (e.g., 150%). This is the mechanism we use to keep the stablecoin pegged. If the price of ADA goes up, you can mint more. If it goes down, you have to add more collateral or burn stablecoins; If you don't, someone else will liquidate your position and get that extra value at your expense.

This mechanism makes it so the value of the stablecoin is dependent on the locked collateral. Because the collateral is in ADA, if we only do this with the two previously mentioned validators, we'll be pegged to the value of ADA. To peg our stablecoin to a fiat currency, we use an oracle that keeps the USD/ADA rate up-to-date to calculate the amount of collateral needed. That way, the collateral depends on the price of USD, and so does our stablecoin.

Let's see how all this works in practice:

### Deploying the stablecoin

The stablecoin developer has to provide:

-   One "Oracle" validator with the USD/ADA rate (the price of ADA in USD).
-   The "Collateral" validator that locks the collateral (ADA).
-   The "Minting" policy to mint/burn stablecoins based on the collateral.

These are the transactions to deploy the oracle to the blockchain (square means Tx, rounded means UTxO):

1. First, we mint an NFT.
2. Once we have that NFT, send it to the oracle's address with the current USD/ADA rate.

```mermaid
flowchart LR
    subgraph Deploy [Deploy Oracle]
    direction LR
    A1((<b>Stablecoin developer</b>)) --> A2(Mint NFT)
    A2 --> A3((<b>Stablecoin developer</b> <br> <sub>NFT</sub>))
    A3 -->|"<sub>NFT <br> + <br> USD/ADA Rate  &#8199 &#8199</sub>"| A4(Deploy Oracle)
    A4 --> A5(("&#8199 &#8199 &#8199 <b>Oracle Validator</b> &#8199 &#8199 &#8199 <br> <sub>NFT <br> Datum: USD/ADA rate</sub>"))
    A4 --> A6((<b>Stablecoin developer</b>))
    end
```

To deploy the "Collateral" and "Minting" validators, we need to submit a single transaction to attach them as reference scripts to the "Always False" validator (or a developer-controlled) address.

```mermaid
flowchart LR
    subgraph Deploy [Deploy Collateral validator and Minting policy]
    direction LR
    A1((<b>Stablecoin developer</b>)) --> A7(&#8199 Deploy Collateral Validator &#8199 <br> and Minting Policy <br> as reference scripts)
    A7 --> A8((<b>Always False Validator</b> <br> <sub>Collateral Validator</sub>))
    A7 --> A9((<b>Always False Validator</b> <br> <sub>Minting Policy</sub>))
    end
```

We're ready to mint stablecoins. But, as soon as we deploy the oracle, its value is outdated. We need a mechanism to keep it up to date.

### Updating the Oracle

To keep the oracle up-to-date, we need a process in our backend checking the current USD/ADA rate from a reliable and trusted source. Because this is for learning purposes, we'll use a simple API.

As soon as the real rate differs from ours, our backend will automatically create a transaction to update our oracle:

```mermaid
flowchart LR
    subgraph Deploy [Update Oracle]
    direction LR
    A1((<b>Stablecoin developer</b>)) --> A3(Update oracle)
    A2(("&#8199 &#8199 &#8199 <b>Oracle Validator</b> &#8199 &#8199 &#8199 <br> <sub>NFT <br> Datum: <b>OLD</b> USD/ADA rate</sub>")) --> A3
    A3 --> A4((<b>Stablecoin developer</b>))
    A3 --> A5(("&#8199 &#8199 &#8199 <b>Oracle Validator</b> &#8199 &#8199 &#8199 <br> <sub>NFT <br> Datum: <b>NEW</b> USD/ADA rate</sub>"))
    end
```

### Minting Stablecoins

To obtain our stablecoins, we use the "Collateral" validator to lock our ADA and the "Minting" policy to mint the tokens. The stablecoin developer chose to overcollateralize by 50% (we have to lock 150% of the value minted). So, if the USD/ADA rate is 1, and we lock 150 ADA (150 USD), we'll be able to mint, at most, 100 stablecoins (100 USD).

These are the transactions to mint some stablecoins:

```mermaid
flowchart LR
    subgraph Mint [Mint Stablecoin]
    direction LR
    A6(("&#8199 &#8199 <b>Oracle Validator</b>&#8199 &#8199  <br> <sub>NFT <br> Datum: USD/ADA rate</sub>"))
    A9((<b>Always False Validator</b> <br> <sub>Minting Policy</sub>))

    M1((&#8199 &#8199 <b>User 1</b> &#8199 &#8199<br> <sub>Collateral</sub>)) --> |Locks ADA as collateral| M4(Mint stablecoins)
    A9 --> |As reference script| M4
    A6 --> |As reference Input|M4

    M4 --> M5(("&#8199 &#8199&#8199 &#8199 <b>Collateral Validator</b>&#8199 &#8199 &#8199 &#8199 <br> <sub>Collateral <br> <br> Datum: <br> Collateral owner  <br> Amount of stablecoin minted"))
    M4 --> M6((&#8199 &#8199 <b>User 1</b> &#8199 &#8199 <br> <sub>Stablecoins</sub>))
    end
```

### Burning Stablecoins

We (the user that minted the stablecoins) burn the stablecoins to unlock our collateral and get it back.
In this case, because we're consuming a UTxO from the collateral validator, we have to provide it as refrence script aswell.

```mermaid
flowchart LR
    subgraph Burn [Burn Stablecoin]
    direction LR
    A8((<b>Always False Validator</b> <br> <sub>Collateral Validator</sub>))
    A9((<b>Always False Validator</b> <br> <sub>Minting Policy</sub>))
    M5(("&#8199 &#8199 &#8199 &#8199 <b>Collateral Validator</b>&#8199 &#8199 &#8199 &#8199 <br> <sub>Collateral <br> <br> Datum: <br> Collateral owner: User 1  <br> Amount of stablecoin minted"))
    M6((&#8199 &#8199 <b>User 1</b> &#8199 &#8199 <br> <sub>Stablecoins</sub>))

    M6 --> B4(Unlock Collateral <br> + <br> Burn stablecoin)
    A8 --> |As reference script| B4
    A9 --> |As reference script| B4
    M5 --> B4
    B4 --> B6((&#8199 &#8199 <b>User 1</b> &#8199 &#8199<br> <sub>Collateral</sub> ))
    end
```

### Liquidating a position

Finally, if the USD/ADA in the up-to-date oracle changes in a way that the value in our locked collateral is lower than 150% of the minted value (let's say 130%), anyone with enough stablecoins can liquidate our position.

When a user liquidates someone else's position, it has to burn the same amount of stablecoins that were minted when the collateral was locked. But, because there's more value locked (in this example, 30% more) than the value of the stablecoins burned, the liquidator takes the difference home.

This means that if the total value of the locked collateral is between 101% and 149%, there's an incentive for someone else to mint their own (correctly collateralized) stablecoins and use them to liquidate that position. Maintaining the stablecoin price stable while making a profit.

```mermaid
flowchart LR
   subgraph Burn [Liquidate Stablecoin]
   direction LR
   A6((" &#8199 &#8199 <b>Oracle Validator</b> &#8199 &#8199  <br> <sub>NFT <br> Datum: USD/ADA rate </sub>"))
   A8((<b>Always False Validator</b> <br> <sub>Collateral Validator</sub>))
   A9((<b>Always False Validator</b> <br> <sub>Minting Policy</sub>))
   M5(("&#8199 &#8199 &#8199 &#8199 <b>Collateral Validator</b> &#8199 &#8199 &#8199 &#8199  <br> <sub>Collateral <br> <br> Datum: <br> Collateral owner: User 1  <br> Amount of stablecoin minted"))
   M6(("&#8199 &#8199 <b>User 2</b> &#8199 &#8199 <br> <sub>Stablecoins</sub>"))

   M6 --> B4(Unlock Collateral <br> + <br> Burn stablecoin)
   A8 --> |As reference script| B4
   A9 -->|As reference script| B4
   M5 --> B4
   A6 --> |As reference Input|B4
   B4 --> B6(("&#8199 &#8199 &#8199 &#8199 <b>User 2</b>&#8199 &#8199 &#8199 &#8199 <br> <sub>User's 1 Collateral</sub>"))
   end
```

### Shutting down the stablecoin

If the developer wants to call it quits, he can shut down the stablecoin by deleting the oracle. By doing that, users won't be able to mint more stablecoins or liquidate one another, but they'll be able to burn their own stablecoins and get their collateral back.

Transaction to shut down the stablecoin:

```mermaid
flowchart LR
    subgraph Deploy [Delete Oracle]
    direction LR
    A1((<b>Stablecoin developer</b>)) --> A3(Delete oracle)
    A2(("&#8199 &#8199 <b>Oracle Validator</b> &#8199 &#8199 <br> <sub>NFT <br> Datum: USD/ADA rate</sub>")) --> A3
    A3 --> A4((<b>Stablecoin developer</b> <br> <sub>NFT</sub>))
    end
```

And that's it! Feel free to take a look at the implementation of this algorithm in the `on-chain` folder!
