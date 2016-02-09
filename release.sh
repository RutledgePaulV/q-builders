#!/usr/bin/env bash

read -p "This will reset your current working tree to origin/develop, is this okay? " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]
then
    git fetch
    git reset --hard origin/develop

    echo "Creating the release branch"
    mvn jgitflow:release-start -DpushReleases=true -DautoVersionSubmodules=true

    echo "Merging the release branch into develop & master, pushing changes, and tagging new version off of master"
    mvn jgitflow:release-finish -DnoReleaseBuild=true -DpushReleases=true -DnoDeploy=true

    echo "Checking out latest version of master."
    git fetch
    git checkout origin/master

    echo "Deploying new release artifacts to sonatype repository."
    mvn clean deploy -P release

    read -p "Would you like to promote the release from the sonatype repository to the central repository?"
    echo

    if [[ $REPLY =~ ^[Yy]$ ]]
        read -p "Oh, Okay. Would you like to drop the version deployed to the snapshot repository?"
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]
            exit 0;
        then
            mvn nexus-staging:drop -P release
        fi
    then
        mvn nexus-staging:release -P release
    fi

fi
