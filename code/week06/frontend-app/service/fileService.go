package service

import (
	"fmt"
	"io/ioutil"
	"log"
	"os"
	"regexp"
)

func readWallets() map[string]string {
	walletFiles, err := readDirectory()
	if err != nil {
		log.Fatalf("failure reading directory %s", err)
	}
	wallets := make(map[string]string, 0)
	for _, file := range walletFiles {
		var content string
		regex := regexp.MustCompile(`^\d*$`)
		if regex.Match([]byte(file)) {
			content = readFile("W" + file + ".cid")
		} else {
			content = readFile(file + ".cid")
		}

		wallets[file] = content
	}
	return wallets
}

func readDirectory() (wallets []string, err error) {
	cidRootDirectory := os.Getenv("CID_ROOT")
	if cidRootDirectory == "" {
		cidRootDirectory = "../"
	}
	file, err := os.Open(cidRootDirectory)
	defer file.Close()
	list, err := file.Readdirnames(0)
	wallets = make([]string, 0, len(list))
	re := regexp.MustCompile(`W(\d*)\.cid`)
	for _, name := range list {
		result := re.FindAllStringSubmatch(name, 1)

		if len(result) > 0 {
			var fileName string
			if result[0][1] != "" {
				fileName = result[0][1]
			}
			wallets = append(wallets, fileName)
		}

	}
	return wallets, err

}

func readFile(filename string) string {
	cidRootDirectory := os.Getenv("CID_ROOT")
	if cidRootDirectory == "" {
		cidRootDirectory = ".."
	}
	data, err := ioutil.ReadFile(cidRootDirectory + "/" + filename)
	if err != nil {
		fmt.Println("File reading error", err)
		return ""
	}
	return string(data)
}
