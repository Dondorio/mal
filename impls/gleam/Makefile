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

$(STEPS):
ifneq ($(filter $(GLEAM_TARGET), javascript js), )
	npm install ../gleam
endif
	gleam build --no-print-progress

clean:
	gleam clean
	rm -rf node_modules
