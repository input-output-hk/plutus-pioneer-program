# Using Docker

In this guide, you'll learn how to set up a local working environment using Docker and Visual Studio Code. Either if have previous experience using Docker or not, we will guide you step by step from installing the software you need, through testing your working environment.

## Installing Docker

[Docker](https://www.docker.com/) is a platform designed to help developers build, share, and run applications into an isolated environment on any operating system. 

To ease setting up a local working environment for this course, the IOG Education Team created a Docker container that packages all the dependencies required to follow up the lessons of this program

{% hint style="info" %}
A **Docker container** is a standard unit of software that packages up code and all its dependencies so and application runs quickly and reliably from one computing environment to another. You can learn more about Docker containers in [this page](https://www.docker.com/resources/what-container/) from the Docekr documentation.
{% endhint %}

Follow the next steps to install Docker in your computer.

1. Open your browser and navigate to <https://www.docker.com/>. From the Docker's homepage, click on the "Download Docker Desktop" button. By default, you'll download a version compatible with you operating system. Docker is available for Linux, Microsoft Windows, and Apple macOS.

    {% hint style="warning" %}
    **Important note for macOS users.** Be sure that you download the correct version according to the chip of your computer, for M1 or M2 chips, [download the "Apple Chip" version](https://desktop.docker.com/mac/main/arm64/Docker.dmg). For Intel chips, [download the "Intel Chip" version](https://desktop.docker.com/mac/main/amd64/Docker.dmg).
    {% endhint %}

    In the image below, you can see how the download button looks on a computer using Microsoft Windows. We'll this operating system for the purposes of this guide.

    ![Docker homepage where the Download Docker Desktop button is highlighted.](images/docker-guide-01.png)

2. After downloading the Docker Desktop installer, execute it and follow the instructions by choosing the default options. Installation options may vary depending on your chip and operating system. If you need detailed instructions, please visit the [Get Docker section](https://docs.docker.com/get-docker/) in the docker docs website.

    {% hint style="info" %}
    After installing Docker Desktop, it'll automatically start after login into your computer. You can change this behavior by turning off the "Start Docker Desktop when you log in" in the Docker Settings configuration. The following image shows the general settings in Docker Desktop running in Windows.

    ![Docker Desktop general settings in a Windows computer.](images/docker-guide-02.png)

    To learn more about changing the settings in Docker Desktop, please visit the ["Change Settings" section](https://docs.docker.com/desktop/settings/mac/) of the Docker Desktop manual.
    {% endhint %}

Now that you have Docker Desktop up and running, let's download and install Visual Studio Code.

## Installing Visual Studio Code

[Visual Studio Code](https://code.visualstudio.com/), also known as VS Code, is a source code editor freely distributed by Microsoft that runs on Windows, Linux and macOS. Additionally to allow code editing, VS Code allows developers to create and install extensions that eases their daily work.

Follow the next steps to install VS Code and a handy extension that you will use in this course.

1. Open your browser and navigate to <https://code.visualstudio.com/> to open Visual Studio Code website. As the image below shows, there is a button where you 
can download this software. Depending on your operating system, the button's name will change. Be sure to download the latest version.

    {% hint style="info" %}
    To download VS Code for any operating system, please visit the [Download Visual Studio Code page](https://code.visualstudio.com/Download) and choose the operating system of your preference.
    {% endhint %}

    For this demo, the button to download VS Code for Windows is highlighted.

    ![Visual Studio homepage where the button to download this software for Windows is highlighted.](images/docker-guide-03.png)

2. After downloading the VS Code installer, execute it and follow the instructions by choosing the default options. Installation options may vary depending on your chip and operating system. If you need detailed instructions, please visit the [Setting up Visual Studio Code section](https://code.visualstudio.com/docs/setup/setup-overview) in the VS Code docs website.

3. Once you installed VS Code, open it to install a extension. Extensions are additional add-ons that extends the VS Code's functionality; some extensions are provided by Microsoft, but also, there are plenty of extensions created by other companies and developers. To install a extension, click on the "Manage" icon in the bottom left corner and choose the "Extensions" options at the image below shows.

    ![Visual Studio UI where the Extensions option is highlighted in the Manage menu.](images/docker-guide-04.png)

4. Next, a window showing the "Extensions Marketplace" will appear on the left side of the VS Code UI. In the search box type `remote development` to look for the [Remote Development extension provided by Microsoft](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.vscode-remote-extensionpack). Once the extension appears, click on it. As the image below shows, to install a extension you should click on the "Install" button.

    ![Visual Studio Extensions Marketplace where the Remote Development extension is highlighted.](images/docker-guide-05.png)

5. After successfully installing the Remove Development extension, you'll note that a "Uninstall" button appears in the extension description tab, and the "Open a Remote Window" icon will appear on the bottom left corner of the VS Code UI as you can see in the following image.

    ![Visual Studio UI after a successful installation of the Remote Development extension.](images/docker-guide-06.png)

Awesome, you have installed all the required software to use the Docker container! Now, we'll guide you through the steps you need to follow to load the Docker container and execute it for the first time.

## Running the PPP Docker Container

Before moving forward into using the Docker container provided for this cohort of the PPP, you need to close VS Code. Next, please follow the next steps.

### Cloning the PPP Repository