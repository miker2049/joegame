#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash openjdk8
for file in $(find ~/joegame/packages/sentence-cluster/stanford-corenlp-4.5.4 -name "*.jar"); do
    export CLASSPATH="$CLASSPATH:$(realpath $file)"
done
# java -mx4g edu.stanford.nlp.pipeline.StanfordCoreNLPServer -port 9000 -timeout
java -mx3g edu.stanford.nlp.pipeline.StanfordCoreNLP -outputFormat json -file $@
