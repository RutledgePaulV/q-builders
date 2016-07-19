#!/bin/bash

function unit_tests() {
    mvn clean test
}

function integration_test() {
    mvn clean install failsafe:integration-test
}

function snapshot() {
    read -p "This will reset your current working tree to origin/develop, is this okay? " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
    git fetch
    git reset --hard origin/develop

    echo "Deploying new release artifacts to sonatype repository."
    mvn clean deploy -P release
    fi
}


function release() {
    read -p "This will reset your current working tree to origin/develop, is this okay? " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]
    then
        git fetch
        git reset --hard origin/develop

        echo "Creating the release branch"
        mvn -Prelease jgitflow:release-start -DpushReleases=true -DautoVersionSubmodules=true

        echo "Merging the release branch into develop & master, pushing changes, and tagging new version off of master"
        mvn -Prelease jgitflow:release-finish -DnoReleaseBuild=true -DpushReleases=true -DnoDeploy=true

        echo "Checking out latest version of master."
        git fetch
        git checkout origin/master

        echo "Deploying new release artifacts to sonatype repository."
        mvn clean deploy -P release
    fi
}

function upgrade_dependencies() {
    echo "Checking for a newer version of a parent pom.xml file"
    mvn versions:update-parent

    echo "Updating all pom.xml files to the latest available from their respective repositories. No changes will be committed."
    mvn versions:use-latest-releases

    echo "Checking for properties used to manage dependency versions and updating them as well. No changes will be committed."
    mvn versions:update-properties
}

case "$1" in
    integration-test)
        echo -n "Starting integration tests..."
        integration_test
        echo ""
    ;;
    release)
        echo -n "Preparing to release a new version of the app..."
        release
        echo ""
    ;;
    unit-test)
        echo "Starting unit tests..."
        unit_tests
        echo ""
    ;;
    upgrade)
        echo -n "Checking for new versions of dependencies..."
        upgrade_dependencies
        echo ""
    ;;
    *)
        echo "Usage: ./manage.sh integration-test|release|unit-test|upgrade"
        exit 1
esac

exit 0