Week 09 - Property Based Testing
================================

.. note::
    This is a written version of `Lecture
    #9 <https://youtu.be/-RpCqHuxfQQ>`__.

    In this lecture we cover Marlowe - a special-purpose language for financial contracts on Cardano.

    This week we were using Plutus commit ae35c4b8fe66dd626679bd2951bd72190e09a123, the same commit as we used in the last lecture.

Overview
--------

In the previous lectures we have learnt about all the important ingredients for writing a Plutus application.

We have first looked at the extended UTxO model - the accounting model that Cardano uses - and the additions that Plutus brings to it.

Then we have talked about on-chain validation, minting policies, writing off-chain code, we have seen how to deploy smart contracts and also how to test them.

Plutus is a very powerful language. So powerful, in fact, that you can implement other languages on top of it - you can write an interpreter in Plutus for other languages.