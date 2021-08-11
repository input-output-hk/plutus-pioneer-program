# Oracle Demo With GUI extension

## Prerequisites
- cabal and ghc installed
- golang installed
- npm installed
If you havent golang installed you can use docker containers instead

## Run PAB and oracle client
1. `cabal run oracle-pab`
2. `cabal run oracle-client`

## Start the frontend Application
### Compile Frontend Gui code
> you can skip this if you do not want to recompile the frontend code
If you want to recompile the Frontend GUI Code you need to navigate into the frontend-app/oracle-gui-client folder and run `npm run build`
this will compile the html and js code into a static served folder.

### Run the Frontend application
1. Navigate to fronten-app
2. `go run main.go`
3. navigate to [the gui](http://localhost:3001/)

## Running with Docker 
If you do not have npm or golang installed you can simply use the pre build docker image `toky03/oracle-frontent-client:0.0.1`
You need to add the volume to enable the container to grap all *.cid files (To fetch the uuids)
And you need to run the docker container with the host network in order to be able to call all localhost endpoints
Command to run the container
```bash
# from withing the week06 folder
docker run -v ${pwd}/frontend-app:/app --network host --name frontend-client toky03/oracle-frontent-client:0.0.1`
```
