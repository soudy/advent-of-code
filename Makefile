DAYS=$(wildcard */)
DAY?=01 # default day for `run`
ERLFILES=$(wildcard */*.erl)
TARGETFILES=$(patsubst %.erl,%, $(ERLFILES))

BEAMFILES=$(patsubst %.erl,%.beam, $(ERLFILES))
CRASHFILES=$(wildcard */erl_crash.dump)

all: $(TARGETFILES)

$(TARGETFILES): %: %.erl $(ERLFILES)
	erlc -o $(dir $@) $<

$(DAYS):
	@erlc -o $@ $(wildcard $@*.erl)

run: $(DAY)/
	@echo "Running day $(DAY) answers..."
	@cd $(DAY) && \
		erl -run part1 main -run part2 main -run init stop -noshell 2> /dev/null || \
		erl -run bothparts main -run init stop -noshell

clean:
	@rm -f $(BEAMFILES) $(CRASHFILES)

.PHONY: all clean run $(DAYS)
