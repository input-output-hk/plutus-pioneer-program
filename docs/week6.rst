Week 06
=======

.. note::
      These is a written version of `Lecture
      #6 <https://www.youtube.com/watch?v=Tr2VBm8vOhw>`__.

      In this lecture we learn about oracles and using the PAB.

      These notes use Plutus commit 476409eaee94141e2fe076a7821fc2fcdec5dfcb

      
Overview
--------

In this lecture we are going to look at a case study, to see how what we have learned so far can be turned into an actual application. A collection of executables that even come with a little front end.

It will be a real dApp, apart from the fact that we don't have a real blockchain available yet. This will run on a
simulated blockchain - a mockchain.

The example we are going to use for this is to implement a very simple oracle.

.. note::
    In the blockchain world, an oracle is a way to get real-world information onto the blockchain, in order to
    make it usable in smart contracts.

There are numerous examples of use cases for oracles. We can think of external data sources such as weather data, election
results, stock exchange data or randomness. You may have a betting contract that depends on the outcome of a specific
sports game, for example.

There are various ways to implement oracles, of varying sophistication.

We are going to use a very simple approach, where we have one trusted data provider.





