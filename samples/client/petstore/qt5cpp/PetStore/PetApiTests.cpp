#include "PetApiTests.h"

#include <QJsonDocument>
#include <QNetworkAccessManager>
#include <QNetworkReply>
#include <QDebug>

PetApiTests::PetApiTests () {}

PetApiTests::~PetApiTests () {
    exit(1);
}

SWGPetApi* PetApiTests::getApi() {
    SWGPetApi* api = new SWGPetApi();
    api->host = "http://petstore.swagger.io";
    api->basePath = "/v2";
    return api;
}

SWGPet* PetApiTests::createRandomPet() {
    SWGPet* pet = new SWGPet();
    qint64 id = QDateTime::currentMSecsSinceEpoch();

    pet->setName(new QString("monster"));
    pet->setId(id);
    pet->setStatus(new QString("freaky"));

    return pet;
}

void PetApiTests::runTests() {
    PetApiTests* tests = new PetApiTests();
    QTest::qExec(tests);
    delete tests;
}

void PetApiTests::findPetsByStatusTest() {
    QEventLoop loop;
    QTimer timer;
    timer.setSingleShot(true);

    SWGPetApi* api = getApi();

    auto validator = [](QList<SWGPet*>* pets) {
        foreach(SWGPet* pet, *pets) {
            QVERIFY(pet->getStatus()->startsWith("available") || pet->getStatus()->startsWith("sold"));
        }
        loop.quit();
        qDeleteAll(pets);
        delete api;
    };

    connect(api, &SWGPetApi::findPetsByStatusSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    QList<QString> status;
    status.append(QStringLiteral("available"));
    status.append(QStringLiteral("sold"));
    api->findPetsByStatus(status);
    timer.start(14000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}

void PetApiTests::createAndGetPetTest() {
    QEventLoop loop;
    QTimer timer;
    timer.setSingleShot(true);

    // create pet
    SWGPetApi* api = getApi();

    auto validator = [&]() {
        // pet created
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    SWGPet* pet = createRandomPet();
    qint64 id = pet->getId();

    api->addPet(*pet);
    timer.start(14000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // get it
    api = getApi();

    auto getPetValidator = [&](SWGPet* pet) {
        QVERIFY(pet->getId() > 0);
        QVERIFY(pet->getStatus()->compare("freaky") == 0);
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::getPetByIdSignal, this, getPetValidator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}

void PetApiTests::updatePetTest() {
    SWGPet* pet = createRandomPet();
    SWGPet* petToCheck = nullptr;
    qint64 id = pet->getId();
    QEventLoop loop;
    QTimer timer;
    timer.setSingleShot(true);

    // create pet
    SWGPetApi* api = getApi();

    auto validator = [&]() {
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->addPet(*pet);
    timer.start(100000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    api = getApi();

    auto fetchPet = [&](SWGPet* pet) {
        petToCheck = pet;
        loop.quit();
        delete api;
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    api = getApi();

    auto updatePetTest = [&]() {
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::updatePetSignal, this, updatePetTest);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    petToCheck->setStatus(QStringLiteral("scary"));
    api->updatePet(*petToCheck);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // check it
    api = getApi();

    auto fetchPet2 = [&](SWGPet* pet) {
        QVERIFY(pet->getId() == petToCheck->getId());
        QVERIFY(pet->getStatus()->compare(petToCheck->getStatus()) == 0);
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet2);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}

void PetApiTests::updatePetWithFormTest() {
    SWGPet* pet = createRandomPet();
    SWGPet* petToCheck = nullptr;
    qint64 id = pet->getId();
    QEventLoop loop;
    QTimer timer;
    timer.setSingleShot(true);

    // create pet
    SWGPetApi* api = getApi();

    auto validator = [&]() {
        loop.quit();
        delete api;
    };

    connect(api, &SWGPetApi::addPetSignal, this, validator);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);
    api->addPet(*pet);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    api = getApi();

    auto fetchPet = [&](SWGPet* pet) {
        petToCheck = pet;
        loop.quit();
        delete api;
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // update it
    api = getApi();

    connect(api, &SWGPetApi::updatePetWithFormSignal, this, [&](){
        loop.quit();
        delete api;
    });
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->updatePetWithForm(id, new QString("gorilla"), NULL);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");

    // fetch it
    api = getApi();

    auto fetchUpdatedPet = [&](SWGPet* pet) {
        QVERIFY(pet->getName()->compare(QString("gorilla")) == 0);
        loop.quit();
        delete api;
    };
    connect(api, &SWGPetApi::getPetByIdSignal, this, fetchUpdatedPet);
    connect(&timer, &QTimer::timeout, &loop, &QEventLoop::quit);

    api->getPetById(id);
    timer.start(1000);
    loop.exec();
    QVERIFY2(timer.isActive(), "didn't finish within timeout");
}
