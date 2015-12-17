DAYS=$(wildcard */)
ERLFILES=$(wildcard */*.erl)
TARGETFILES=$(patsubst %.erl,%, $(ERLFILES))

BEAMFILES=$(patsubst %.erl,%.beam, $(ERLFILES))
CRASHFILES=$(wildcard */erl_crash.dump)

all: $(TARGETFILES)

$(TARGETFILES): %: %.erl $(ERLFILES)
	@erlc -o $(dir $@) $<

$(DAYS):
	@erlc -o $@ $(wildcard $@*.erl)

clean:
	@rm -f $(BEAMFILES) $(CRASHFILES)

.PHONY: all clean $(DAYS)
