FAQ : Native Tokens (Cardano’s Multi-Asset Support Feature)
===========================================================

On-chain assets
####################

What's the definition of 'multi-asset (MA)' support and does Cardano have it?
********************************************************************************

Multi-asset (MA) support is the name of a feature set (functionality) that a ledger (/blockchain/wallet/cryptocurrency/banking platform) can provide, which allows it to do accounting on or transact with more than one type of asset.

Cardano’s MA support feature is called Native Tokens. MA allows users to transact with ada and an unlimited number of user-defined tokens. This support is native, which means that tokens can be transacted with (tracked/sent/received) using the accounting system defined as part of the ledger functionality of the cryptocurrency, without the need for smart contracts to enable this functionality.

What is (asset) tokenization?
********************************************************************************

Tokenizing an asset means creating an on-chain representation of that asset.


Minting
############

What does 'minting' a token mean?
********************************************************************************

‘Minting’ refers to the process whereby new tokens are created or destroyed. That is, the total amount in circulation (ie. added up over all addresses on the ledger)
of the token type being minted increases or decreases. Minting a positive quantity of
tokens is token creation, and minting a negative quantity is token destruction.

What does 'burning' a token mean?
********************************************************************************

‘Burning’ refers to the process whereby tokens are destroyed.
It is synonymous with 'negative minting'.

What is token redeeming?
********************************************************************************

Token redeeming is the action of sending tokens back to the issuer to be burned.
This is usually done when the tokens being redeemed no longer have a purpose
on the ledger, and the user or contract in possession of them is not able
(not unauthorized by the minting policy) to burn the tokens.

There may not be any compensation offered for redeeming the tokens (deciding this
is up to the token issuer/minting policy), but the user may choose to do
so anyway to avoid having unusable tokens in their wallet.

What is a minting transaction?
********************************************************************************

Transactions have different structure in the Shelley, ShelleyMA, and Goguen eras, but this structure is the same within a single er The Shelley+MA and Goguen transactions can carry data that specifies what tokens they are minting. The transactions where this piece of transaction data (called the mint field) is non-empty are called minting transactions. These transactions must also carry the minting policies for the tokens they are minting, so that they can be checked during validation.

The result of processing a minting transaction is that the ledger will now additionally contain the assets included in the mint field (minting field) of the transaction.
If the quantity of a particular asset in the mint field is negative, the result is that after processing the transaction, the total quantity of that specific asset on the ledger will be reduced by the amount reflected in the mint field.

Note that a single transaction might mint tokens associated with multiple distinct minting policies. e.g., ``(Policy1, SomeTokens), (Policy2, SomeOtherTokens)``.
Note also that a transaction might simultaneously mint some tokens and burn some other ones.

What is a minting policy?
********************************************************************************

A minting policy is a set of rules used to regulate the minting of assets associated with it (scoped under it). For example, who has control (and under what conditions) over the supply of the currency, and its minting and burning. These rules are about the content of the transaction data of the transaction that is attempting the mint. e.g., a minting policy can require a particular set of keys to have signed the minting transaction.

This set of rules is defined by the user who wishes to create the new asset. For example, a user might wish to only allow themselves to ever mint this particular kind of token. In this case, they would stipulate this in the policy. The node checks adherence to minting policies when a transaction is processed by running the code or checking the relevant signatures. Transaction data must satisfy all the minting policies of all assets the transaction is attempting to mint.

What is a token builder and what is its functionality?
********************************************************************************

A token builder is a piece of software that allows the user to define the tokens to be minted and include them in a minting transaction. It also ensures that the appropriate additional data needed to verify that the transaction is allowed to perform the mint is included in the transaction (see minting policy question below).


Policy examples and ways to define policies
################################################

What is 'multisig' and how is it related to minting policies?
********************************************************************************

The multisig scripting language (which existed prior to the introduction of MA functionality on Cardano) specifies some minimal set of signatures required to allow a transaction to perform a certain action, usually to spend a UTxO entry.

Multisig scripts can also be used to specify the most basic minting policies, that is, the policies that require a specific set of keys to sign the minting transaction. For example, a single-issuer minting policy can be expressed using a multisig script. Note that minting policies are the only types of policy that can be expressed using multisig.

Without Plutus smart contract capability, or any other minting policy language extensions, multisig is the only way to specify a minting policy.

What do Plutus smart contracts have to do with Native Tokens?
********************************************************************************

Minting policies can be written in the Plutus smart contract language. This allows users to express a much wider range of policies than just the single issuer policy expressible using multisig. The one-time minting policy, for example, can be expressed in Plutus (but not just as multisig).

What is a single-issuer minting policy?
********************************************************************************

A single-issuer minting policy specifies that only the entity holding a particular set of keys is allowed to mint tokens under a particular policy. For example, the set of keys specified in the minting policy must have signed the minting transaction. This type of policy can be specified using multisig.

An example of a single-issuer policy use case could be tokens representing baseball cards. This would mean that no new baseball card tokens could be minted without the company’s signatures. Conversely, the policy proves that all the existing cards scoped under this policy have been legitimately minted by the baseball card company.

What is a one-time minting policy?
********************************************************************************

In a one-time minting policy, the complete set of tokens scoped under it is minted by one specific transaction. This means that no more tokens will ever be minted under that policy. This type of policy does require smart contracts and cannot be expressed using multisig.

A use case of a one-time minting policy would be minting ticket tokens for a specific gig. The venue capacity is known ahead of time, so there’ll be no need to ever allow more tickets to be minted.

Multi-asset structure, representation and properties
#########################################################

What is fungibility and non-fungibility?
********************************************************************************

Fungibility is a relation between two assets/tokens. Tokens are said to be fungible with each other when they are interchangeable. For example, fiat money is fungible as a $10 bill is interchangeable with all other (real) $10 bills (and all 10-sets of $1 bills, and all pairs of $5s).

Non-fungible assets are not interchangeable with each other. For example, two diamonds, or two on-chain tokens representing the two real-world diamonds. If there are no other assets a token is fungible with -such as a token representing a house- the token is deemed to be unique (non-fungible).

What is a token bundle?
********************************************************************************

A mixed collection of tokens scoped under one or more minting policies. Any tokens can be bundled together.

For more detail, see the token bundle section.


Transacting with native tokens
###################################

How do native tokens appear in a user's wallet?
********************************************************************************

Prior to the introduction of MA functionality into the Cardano system, a user’s wallet contains both outputs with
addresses that belong to the user, and the amounts of ada that these addresses hold.
For example, ``(users_address1, someAdaAmount)``

With MA support, the user's wallet will be able to contain multiple types of assets in a single output, i.e., the wallet can contain a token bundle. This means that wallets can contain:

* Assets scoped under different policies in a single UTxO (including ada)
* Assets scoped under one policy, spread over multiple UTxOs

A user’s wallet might contain something like:

  ``(users_address1, (adaPolicy, someAdaTokens))``
  ``(users_address1, (cryptoDoggie, someDoggies),  (adaPolicy, moreAdaTokens))``
  ``(users_address2, (cryptoDoggie, otherDoggies), (cryptoBirds, justCockatoos))``

In this example, there are three policies: ``adaPolicy``, ``cryptoDoggie``, and ``cryptoBirds``.

Do native tokens have human-readable identifiers and other metadata?
********************************************************************************

Human-readable names for assets (instead of the long alphanumeric Policy ID strings and asset names) can be registered on a metadata server. If a user is using a wallet integrated with a metadata server, they will be able to view the human-readable names when looking at their assets.

Users will be able to upload names for their tokens, along with any other metadata pertaining to the specific tokens, to a metadata server. There might be more than one metadata server operational at a time (including one run by Cardano), so users will have to choose which server(s) to upload their metadata to, or to download their metadata from.

Users might also choose to add names and other metadata directly into the metadata field of the transaction. This will increase transaction fees proportionally to the size of the additional metadata.

What are the costs related to minting and trading native tokens?
********************************************************************************

Costs related to multi assets can be divided into two categories:

**Fees**: Sending and minting tokens affects the fees that the author of the transaction must pay. As with an ada-only ledger, the fees are calculated based on the total size of the transaction. There might also be fees for checking minting policies, but initially only multisig policies are supported, which do not incur additional fees on top of the transaction size-based ones.

**Min-Ada-Value**: Every output created by a transaction must include a minimum amount of ada, which is calculated based on the size of the output (that is, the number of different token types in it, and the lengths of their names).

**Min-Ada-Value explanation:**

Recall that outputs may contain a heterogeneous collection of tokens, including ad Ada is a limited resource in the Cardano system. Requiring some amount of ada be included in every output on the ledger (where that amount is based on the size of the output, in bytes) protects the size of the Cardano ledger from growing intractably.

**Min-ada-value calculation:**

The minimum ada amount required to be contained in every ada-only UTxO with no additional data (i.e. a UTxO containing only the address and ada amount) is a parameter the Cardano system: ``minUTxOValue``

The size of such a UTxO has a upper bound : ``adaOnlyUTxOSize``

We can calculate the upper bound on size of a UTxO u containing non-ada tokens : ``sizeBound (u)``

We want to calculate the min-ada required to be contained in u : ``minAda (u)``

A minUTxOValue amount of ada pays for ``adaOnlyUTxOSize`` bytes of UTxO storage on the ledger. To make the min-ada-value proportional for all UTxOs, the following proportion must be satisfied :

	``minUTxOValue / adaOnlyUTxOSize = minAda (u) / sizeBound (u)``

So, the min-ada calculation for any UTxO is:

	``minAda (u) = sizeBound (u) * minUTxOValue / adaOnlyUTxOSize``

As a consequence of this design,

* It is impossible to make outputs containing only custom tokens
* The number of each kind of token in an output does not affect the min-ada-value of the output, but the number of types of tokens contained in an output increases the min-value.
* The reason for this is that the names and policy IDs of each of the types of tokens take up additional space in the output.
* Sending custom tokens to an address always involves sending the min-ada-value of ada to that address alongside the custom tokens (by including the ada in the same output). If the address is not spendable by the user sending the tokens, the ada sent alongside the tokens no longer belongs to the sender.
* Before transferring custom tokens, users may choose to use off-chain communication to negotiate who supplies the ada to cover the min-ada-value in the output made by the transferring transaction
* To recover the ada stored alongside custom tokens in an output O, the user must either:
  a) Spend the output O, and burn the custom tokens stored therein
  b) Spend an output O and an output O’, and consolidate the tokens therein with the same collection of types of custom tokens stored in another output (spent within the same transaction)

Eg. ``(CryptoDoggiesPolicy, poodle, 1)`` contained in O can be consolidated with
``(CryptoDoggiesPolicy, poodle, 3)`` in O’, for a total of ``(CryptoDoggiesPolicy, poodle, 4)`` in a new output made by the consolidating transaction.

* Splitting custom tokens into more outputs than they were contained in before the transaction getting processed requires using, in total, more ada to cover the min-ada-value, as ada is needed in the additional outputs.


What types of assets can I use to cover costs associated with native tokens?
************************************************************************************

Currently, only ada can be used to make fee payments or deposits.

How does coin selection work for custom native tokens?
********************************************************************************

From the users’ perspective, it is similar to ada coin selection, i.e., the user selects the tokens and the quantities that they wish to spend, and the wallet picks appropriate inputs and covers fees.

Is it possible to send tokens to an address?
********************************************************************************

Yes, sending native tokens to an address is done in the same way as sending ada to an address, i.e., by submitting a transaction with outputs containing the token bundles the transaction author wishes to send, together with the addresses to which they are sent.

What control does the user have over custom token assets?
********************************************************************************

Users can spend, send, trade, or receive all types of MA tokens in the same way as ada. Unlike ada, users can
also mint and burn native tokens.

**Spending tokens** : Users can spend the tokens in their wallet, or tokens in outputs locked by scripts that allow this user to spend the output.

**Sending tokens to other users** : Users can send the tokens in their wallets (or any tokens they can spend) to any address.

**Minting tokens** : Users can mint custom tokens according to the policy associated with this asset. The minting transaction can place these tokens in the user’s address, or anyone else’s. If necessary, the policy can restrict the exact output location for the tokens.

Note that even if the user has defined a policy, that user might not be able to mint or burn assets scoped under this policy, depending on the policy rules. A minting policy controls the minting of all assets scoped under it, regardless of the identity of the user who defined the policy.

**Burning tokens** : Burning tokens is also controlled by the policy associated with the asset. Besides being allowed to burn the tokens (always in accordance with the minting policy), the user must also be able to spend the tokens they are attempting to burn. For example, if the tokens are in their wallet).

Users cannot burn tokens over which they have no control, such as tokens in someone else’s wallet, even if the minting policy would specifically allow this.

Is there a Distributed Exchange (DEX) for Cardano Native Tokens?
********************************************************************************

No. The Cardano ledger does not itself support DEX functionality. However, when smart contract functionality is available, one can post non-ada assets for exchange or sale on the ledger using a smart contract.

Is there an asset registry for Cardano Native Tokens?
********************************************************************************

No. The implementation of the Native Tokens feature on Cardano does not require an asset registry. However, the metadata server (see “Do assets have human-readable identifiers and other metadata?”) can be used to list tokens a user has minted, if they wish to do so.


Cardano Native Tokens vs ERC
#################################

How do Cardano native tokens compare to ERC-721 and ERC-20 Ethereum custom tokens?
******************************************************************************************

Cardano’s approach to building custom tokens differs from a non-native implementation of custom tokens, such as ERC-721 or ERC-20, where custom tokens are implemented using smart contract functionality to simulate transfer of custom assets (i.e., a ledger accounting system). Our approach to create custom tokens does not require smart contracts, as the ledger implementation itself supports the accounting on non-ada native assets.

Another key difference is that Cardano multi-asset ledger supports both fungible and non-fungible tokens without specialized contracts (unlike ERC-721 or ERC-20), and is versatile enough to include a combination of different types of fungible and non-fungible tokens in a single output.
