QT += network

HEADERS += \
# Models
    $${PWD}/SWGReturn.h \
# APIs
    $${PWD}/SWGFakeApi.h \
# Others
    $${PWD}/SWGHelpers.h \
    $${PWD}/SWGHttpRequest.h \
    $${PWD}/SWGModelFactory.h \
    $${PWD}/SWGObject.h \
    $${PWD}/SWGQObjectWrapper.h

SOURCES += \
# Models
    $${PWD}/SWGReturn.cpp \
# APIs
    $${PWD}/SWGFakeApi.cpp \
# Others
    $${PWD}/SWGHelpers.cpp \
    $${PWD}/SWGHttpRequest.cpp

