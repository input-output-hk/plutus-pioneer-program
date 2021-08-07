.. _alonzo_white:

Alonzo Walkthrough
..................

These are my notes taken while working with the Alonzo testnet.

The notes take you through the process of setting up an AWS instance running an Alonzo White node, using the command line to 
move ADA between wallets and submitting and executing smart contracts. 

It uses several helper scripts to simplify the creation of the commands. The helper scripts are
simple bash scripts that create ``cardano-cli`` commands and should be easy to understand to get a grip on what is going on behind the scenes.

.. note::
    
    The exercises are only possible if you have some test Ada from the Alonzo White faucet. Test Ada is currently only available to members
    of the Alonzo test group in order to control usage of the network. Alonzo testnet is moving towards a public phase within a matter of weeks.

This is the setup I chose to use, but there are several other ways to run the node and configure the environment.

.. toctree::
    :maxdepth: 2
    :caption: Contents:
    :numbered:

    alonzo/aws_node_setup.rst
    alonzo/wallets_and_funds.rst
    alonzo/always_succeeds_script.rst
    alonzo/always_fails_script.rst
    alonzo/hello_world_script.rst
    alonzo/hello_world_person.rst







