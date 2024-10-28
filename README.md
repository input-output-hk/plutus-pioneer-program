# Plutus Pioneer Program v4 contracts translated to Aiken 

This branch contains a translation of the Plinth (previously called PlutusTx) on-chain code examples from the 
[4th Plutus Pioneer program](https://github.com/input-output-hk/plutus-pioneer-program/tree/fourth-iteration). 
They are re-writen in the Aiken smart contract language. 

| :information_source: | Validators in this repository will compile only with an alpha-version of aiken. The non-alpha versions of aiken have syntax and semantic 
  changes that are not backwards compatible. |
|----------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

To get a basic understanding of the Aiken programming language you can check out the *Language tour* at the official documentation, that 
starts with [Primitive types](https://aiken-lang.org/language-tour/primitive-types). Further chapters can be accessed on the left-side menu. 

The official docs also provide a walkthrough of creating a [Hello World project](https://aiken-lang.org/example--hello-world/basics) that covers: 
* pre-requisites
* scaffolding
* using the standard library
* the first validator
* adding traces
* writing a test
* Lucid and PyCardano off-chain code  

| :information_source: | To see an explanation of the validators in this repository, you can look at the 4th Plutus Pioneer program lectures, that are linked on main README file of the 4th PPP branch. |
|----------------------|:--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|

:hourglass: **Aiken installation instructions:**
* First you will need to install the tooling manager *aikup*.<br>
  For Linux and Mac you can run the command:
  ```console
  curl --proto '=https' --tlsv1.2 -LsSf https://install.aiken-lang.org | sh
  ```
  For Windows you can run the command:
  ```console
  powershell -c "irm https://windows.aiken-lang.org | iex"
  ```
  Up-to-date instructions can be found at the [official page](https://aiken-lang.org/installation-instructions). 
* Once installed you can run the command `aikup list` command that returns a list of available version for the `aiken` CLI, which is used to manage and compile projects. 
  You should then install the latest aplha version with the command `aikup install v1.0.29-alpha`.  
* Aiken comes with a built-in language server. The aiken docs provide [configuration instructions](https://aiken-lang.org/installation-instructions#language-server). 
* Aiken can be integrated with the Zed, VSCode, Vim/Neovim and Emacs editors. Instructions can be found at the following [aiken docs](https://aiken-lang.org/installation-instructions#editor-integrations).  

:pick: **Creating and building an Aiken project:**
* To create a project you can use the command `aiken new --help` that will provide instructions for input parameters. The created project 
  will contain the ***aiken.toml*** file that contains the metadata about the project, as well as dependencies required by it. 
* To check whether all dependencies get download successfully you can use the `aiken check` command. 
* To build an existing aiken project `cd` into that project folder and run `aiken build`. Without input parameters it will compile all validator 
  code that is contained in the files residing in the ***validators/*** folder. 
* The `aiken build` command generates the file ***plutus.json*** at the root of your project that is a [CIP-0057 Plutus blueprint](https://cips.cardano.org/cip/CIP-0057). 
  The blueprint describes the on-chain contract and its binary interface. In particular, it contains the generated on-chain code that will be executed by the ledger, 
  and a hash of the validator(s) that can be used to construct addresses. 
* To get a list of all available aiken actions for the CLI simply run the `aiken` command without arguments. 

:open_book: **Important notes:**
* The ***plutus.json*** file in this repository includes compiled code for all validators from the ***validators/*** folder that was 
  compiled with `Aiken v1.0.29-alpha`. If re-building the project with a newer version of Aiken the compiled code may change. 
  That does not impact the functionality of the code but can improve its performance. 
* The language server protocol (LSP) doesn't work if filename uses camel case:<br> 
  For example ❌ `myValidator.ak` ✅ `my-validator.ak`  

:green_heart: **Aiken and Cardano learning resources:**
* The [Awsome Aiken](https://github.com/aiken-lang/awesome-aiken) repository provides links to Aiken libraries, DApps, tutorials and video lessons.
* You can also check out the [Aiken for Amateurs](https://piefayth.github.io/blog/pages/aiken1/) tutorial, that is currently not include in Awsome Aiken. 
* If you are starting with your Cardano journey you might want to check out the [What I wish I knew](https://aiken-lang.org/fundamentals/what-i-wish-i-knew) 
  page that provides tips and secret knowledge and the [EUTXO chrash course](https://aiken-lang.org/fundamentals/eutxo) that explains the EUTXO accounting model. 
* Also the PPP4 lectures [Hashing and Digital Singatures](https://youtu.be/f-WKPWbk9Jg) and [The EUTXO model](https://youtu.be/ulYDNaEKf4g) provide some 
  beginner friendly explanations. 
  
