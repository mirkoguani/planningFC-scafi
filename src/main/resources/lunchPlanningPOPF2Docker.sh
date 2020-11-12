#docker exec -ti d33 sh -c "cd /app/popf2Planner && ./planPopf2 /app/scenariosPlanner/ma/taxi/p23/domainS.pddl /app/scenariosPlanner/ma/taxi/p23/problemS.pddl outputp23 && cat ./outputPopf2/outputp23.1"
echo "input parameters :"
idcontainer=$1;
pathplan=$2;
namedomainfile=$3;
nameproblemfile=$4;
echo $idcontainer;
echo $pathplan;
echo $namedomainfile;
echo $nameproblemfile

docker start $idcontainer &&
echo "Container with id ${idcontainer} started"
#NB remember to create the folders scalaInputPlan and scalaOutputPlan in Docker !!! : I did it with these following two lines.
docker exec -ti $idcontainer sh -c "rm -rf /app/scalaInputPlan && mkdir -p /app/scalaInputPlan" &&
echo "Folder scalaInputPlan successfully created on Docker" &&
docker exec -ti $idcontainer sh -c "rm -rf /app/scalaOutputPlan && mkdir -p /app/scalaOutputPlan" &&
echo "Folder scalaOutputPlan successfully created on Docker" &&
docker cp "/C${2}${3}" $idcontainer:/app/scalaInputPlan/domainS.pddl &&
echo "Successful copying of the domainS.pddl file from host to container" &&
docker cp "/C${2}${4}" $idcontainer:/app/scalaInputPlan/problemS.pddl &&
echo "Successful copying of the problemS.pddl file from host to container"
echo "Planning started at the time $(date +"%T")" &&
docker exec -ti $idcontainer sh -c "cd /app/popf2Planner && ./planPopf2 /app/scalaInputPlan/domainS.pddl /app/scalaInputPlan/problemS.pddl outputPlan" &&
echo "Planning completed at the time $(date +"%T")" &&
docker cp $idcontainer:/app/scalaOutputPlan/outputPlan "/C${pathplan}outputPlan" &&
echo "Successful copying of the outputPlan file from container to host" &&
docker stop $idcontainer  || read -s -n 1 -p "ERROR!! Read to find what went wrong and then Press any key to continue..."

