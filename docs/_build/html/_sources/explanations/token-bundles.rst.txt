Token Bundles
=============

Terminology
####################

**Asset**

	An object of value which we may wish to track on the blockchain. Such an object can be a variety of things, eg. a digital asset such as ada, a role, a credential, a quantity of a good, etc. This term can refer to both

* the identifier of the object of value, such as "ada is an asset", or
* a particular quantity of a specific object, such as "this house is an asset", or "these 10 tonnes of coffee" are an asset

**Token**

	Short for "asset token", the on-chain representation of an asset.

**Policy**

	Short for "minting policy", it is the set of rules that governs the token-minting operation for all assets associated with that specific policy. We do not see the actual policies in ``TB_Example`` below, only the references to them, ie. their IDs. The association of an asset to its minting policy is permanent, and made at the time of issuance of the asset, necessarily under that very policy.

**Policy ID**

	The unique identifier associated with a minting policy. In the example ``TB_Example``, there are two policy IDs:

	``NFLPlayerCardsPolicyID`` and ``RushConcertPolicyID``.

The ID is computed by applying a hash function to the policy itself, and is thus a sequence of letters and numbers, eg.

	``NFLPlayerCardsPolicyID = e0d123e5f316bef7``

**Asset Name**

	An (immutable) property of an asset used to distinguish assets scoped under the same policy. In ``TB_Example``, the names for the assets with policy ID ``RushConcertPolicyID`` are:

	``Tickets`` and ``VIPTickets``

Unlike policy ID, the asset name does not refer to any code or set of rules, and can be human readable words. However, the policy under which an asset is scoped can specify some constraints on what the asset names under this policy can be.

Different policies can have the same asset names for tokens scoped under them, eg. the token bundle

	``FAKERushConcertPolicyID { (Tickets, 500), (VIPTickets, 50) }``

contains ``Tickets`` and ``VIPTickets`` asset names, but these are not fungible with real ``RushConcertPolicyID`` tickets in the ``TB_Example`` token bundle, as they are scoped under different policies.


**Asset ID**

	The pair of a policy ID and an asset name. Tokens with the same asset ID have the property that they are all fungible with each other, and not fungible with tokens with a different asset ID. An asset ID is the unique identifier for a collection of tokens with this property. A policy may have assets scoped under it which have different asset names, eg.

	``RushConcertPolicyID { (Tickets, 500), (VIPTickets, 50) }``

is a token bundle with assets scoped under a single policy ID ``RushConcertPolicyID``, but two asset names, and therefore two asset IDs. They are

	``(RushConcertPolicyID, Tickets)``, and ``(RushConcertPolicyID, VIPTickets)``

Intuitively, all regular tickets are fungible with each other, and all VIP tickets are fungible with each other. But a VIP ticket is not fungible with a regular ticket, as they have different asset IDs.

**Quantity**

	The number of tokens with a particular asset ID, specified in a token bundle. For example, there are 500 tokens with asset ID

	``(RushConcertPolicyID, Tickets)``

in ``TB_Example``. That is, there are 500 non-vip Rush concert tickets in this bundle.

Token bundle
####################

Definition
***********

A token bundle is a heterogeneous (mixed) collection of tokens. Any tokens can be bundled together. Token bundles are the standard - and only - way to represent and store assets on the Cardano blockchain.

Token bundles organize tokens into a particular kind of data structure (see example and explanation below), so that which tokens are fungible with which other tokens explicitly follows from this organization.

In previous versions of the Cardano ledger, ada amounts were specified in transaction and UTxO outputs. With the introduction of multi-asset support, these ada amounts have been replaced with token bundles, which can specify an ada amount alongside quantities of other assets in a single output.

Token bundles are contained in outputs and mint fields of transactions, and the outputs in the UTxO set tracked by the ledger. Note that certain fields of a transaction must still explicitly specify ada amounts, such as the fee field.

Token Bundle Example
**********************

This is an example of a token bundle, let’s call it ``TB_Example`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeNFLPlayerCard, 1),
			(SomeOtherNFLPlayerCard, 1),
			(YetAnotherNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 500),
			(VIPTickets, 50)}
	}

We will use this example to explain the terminology below.

How and where are token bundles stored?
********************************************

Token bundles can be found:
* As the mint field of a transaction, indicating that the transaction is minting the tokens in the bundle.
* In an output of a transaction or an output in the current UTXO tracked by the ledger, alongside the address of the output, ie.

	``{ MyAddress, TB_Example }``

Splitting and Combining Token Bundles
********************************************

Transactions can arbitrarily split and combine token bundles into different bundles. Note that assets with the same ID are always fungible with each other, even when contained in separate bundles. For example, we can split the bundle ``TB_Example`` into two:

``TB_Example_Part1`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 200),
			(VIPTickets, 20)}
	}

``TB_ExamplePart2`` : ::

	{
		NFLPlayerCardsPolicyID {
			(SomeOtherNFLPlayerCard, 1),
			(YetAnotherNFLPlayerCard, 1)}

		RushConcertPolicyID {
			(Tickets, 300),
			(VIPTickets, 30)}
	}
