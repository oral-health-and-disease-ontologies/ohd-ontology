export trunk=`pwd`/../..
save=`pwd`
cd $trunk
export trunk=`pwd`
cd $save
# This is where the database files will be kept (not checked in, of course)
export STARDOG_HOME=$trunk/store/stardog-r21/

# install the stardog distribution somewhere and then soft link in the executables stardog-admin, stardog-shell, and stardog into trunk/bin
export PATH=$trunk/bin:$PATH
export STARDOG_JAVA_ARGS='-DdisableSecurity=true -Xmx4g'
export ontfiles="$trunk/src/ontology/pitt-ub-ohsu-r21/imports/*.owl $trunk/src/ontology/pitt-ub-ohsu-r21/imports/BFO2/*.owl $trunk/src/ontology/pitt-ub-ohsu-r21/ohd.owl"

# instance files are kept in a dropbox folder rather than in repo as they have might have phi, and soft linked into this folder
export instancefiles="$trunk/src/ontology/pitt-ub-ohsu-r21/dropbox/*.owl"

# Reasoning expressivity for stardog
export expressivity=RL

#echo STARDOG_HOME=$STARDOG_HOME
#echo PATH=$PATH
#echo expressivity=$expressivity
