STEPS = \
step0_repl \
step1_read_print \
step2_eval \
step3_env \
step4_if_fn_do \
step5_tco \
step6_file \
step7_quote \
step8_macros \
step9_try \
stepA_mal

gleam_MODE ?= erlang

$(STEPS):
ifneq ($(filter $(gleam_MODE), javascript js), )
	npm install
endif
	gleam build --target $(gleam_MODE)

clean:
	gleam clean
	rm -rf node_modules
