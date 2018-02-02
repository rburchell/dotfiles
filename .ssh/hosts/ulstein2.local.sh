#!/bin/sh

# gui editor
#export QML2_IMPORT_PATH=~/code/ulstein/Ias2Gui/products/ias-gui/app/qml/imports

# everything
export NDDSHOME=/opt/rti/rti_connext_dds-5.2.3/

# gui
export QT_QUICK_BACKEND=software

# Control System
export LD_LIBRARY_PATH=$NDDSHOME/lib/x64Linux3gcc4.8.2
export OPT_QT=~/Qt/5.9/gcc_64/bin/
export UCS_GIT_PATH=~/code/ulstein/UlsteinControlSystem/
export UCS_GIT_BIN=$UCS_GIT_PATH/bin/
export NET_SNMP_PATH="/opt/net-snmp/"

# ccache stuff
export PATH="/usr/lib/ccache:$PATH"
