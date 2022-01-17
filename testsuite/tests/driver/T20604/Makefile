TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# Test for passing -dynamic and -dynamic-too together

clean:
	rm -f *.o
	rm -f *.hi
	rm -f *.dyn_o
	rm -f *.dyn_hi

T20436: clean
	'$(TEST_HC)' $(TEST_HC_OPTS) A.hs
	'$(TEST_HC)' $(TEST_HC_OPTS) --show-iface A1.hi | grep addDependentFile || :
