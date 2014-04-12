PROJECT = carthage

DEPS = ranch
dep_ranch =  https://github.com/extend/ranch 0.8.4

TEST_DEPS = meck
dep_meck = https://github.com/eproxus/meck 0.7.2

include erlang.mk
