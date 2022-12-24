KOTLINC_PATH=${HOME}/Downloads/kotlin/kotlinc
KOTLINC=$(KOTLINC_PATH)/bin/kotlinc

.PHONY: run
run: out
	java -cp "$(KOTLINC_PATH)/lib/*:out" MainKt
out: Main.kt alert
	$(KOTLINC) Main.kt -d out

.PHONY: alert
alert:
	@echo "Make sure you set the KOTLINC_PATH variable in the makefile"
	@echo
