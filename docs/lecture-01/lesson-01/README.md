# Lesson 1.1: The EUTxO Model

Welcome to Lesson 1.1! In this lesson you'll learn about the EUTxO model and how it operates on Cardano.

{% embed url="https://youtu.be/bfofA4MM0QE" %}

{% tabs %}
{% tab title="Learning Objectives" %}
After completing this lesson, you'll be able to:

* Describe the EUTxO model.
* Exemplify how transactions work on Cardano using EUTxO.
* Explain the advantages that EUTxO adds to Cardano for securing transactions.
{% endtab %}

{% tab title="Lesson Resources" %}

* [Lesson slides](#).

{% endtab %}

{% tab title="Additional Resources" %}
To learn more about the topics covered in this lesson, we encourage you to review the following resources:

* [Understanding the Extended UTxO model](https://docs.cardano.org/learn/eutxo-explainer)
* [EUTxO Handbook](https://ucarecdn.com/3da33f2f-73ac-4c9b-844b-f215dcce0628/EUTXOhandbook\_for\_EC.pdf)
{% endtab %}
{% endtabs %}

### Frequently Asked Questions

<details>

<summary>How EUTxO extends the UTxO model?</summary>

The EUTxO model extends the UTxO model in two ways:

1. It generalizes the concept of ‘address’ by using the lock-and-key analogy. Instead of restricting locks to public keys and keys to signatures, addresses in the EUTxO model can contain arbitrary logic in the form of scripts. For example, when a node validates a transaction, the node determines whether or not the transaction can use a certain output as an input. The transaction will look up the script provided by the output's address and execute the script if the transaction can use the output as an input.
2. The second difference between UTxO and EUTxO is that outputs can carry (almost) arbitrary data in addition to an address and value. This makes scripts much more powerful by allowing them to carry state information.

</details>

<details>

<summary>What are the advantages of EUTxO?</summary>

Cardano’s EUTxO model provides a secure and versatile environment to process multiple operations without system failures. This model offers better scalability and privacy, as well as more simplified transaction logic, as each UTxO can only be consumed once and as a whole, which makes transaction verification much simpler.

</details>

