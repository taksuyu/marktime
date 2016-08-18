node {
  stage 'git'
  checkout scm

  stage 'stack'

    // We set the TMPDIR env var so that stack doesn't try to build everything
    // in /tmp and run out of resources.
    withEnv(['TMPDIR=$HOME/tmp']) {
      sh 'stack test'
    }
}
