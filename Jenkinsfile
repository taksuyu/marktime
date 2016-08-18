node {
  stage 'git'
  step 'clone git repo'
  checkout scm

  step 'search for stack files'
  for (config in findFiles(glob: 'stack*.yaml')) {

    stage 'stack - $(config)'
    // We set the TMPDIR env var so that stack doesn't try to build everything
    // in /tmp and run out of resources.
    withEnv("TMPDIR=~/tmp") {
      sh 'stack test --stack-yaml $(config)'
    }
  }}
