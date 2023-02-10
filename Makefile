.PHONY := build
build:
	dune build

.PHONY := switch
switch:
	opam switch create . --deps-only --with-test --with-doc

.PHONY := update-deps
update-deps:
	opam install . --deps-only

.PHONY := fmt
fmt:
	dune fmt

.PHONY := repl
repl:
	dune exec feint

.PHONY := run
run-example:
	dune exec feint example.fi

# Parser message file generation/maintenance ---------------------------

parser_file := lib/parser.mly
message_file := lib/parserMessages.messages
backup_message_file = /tmp/parserMessages.messages.bak
updated_message_file = /tmp/parserMessages.updated
auto_message_file = /tmp/parserMessages.auto
merged_message_file = /tmp/parserMessages.merged

# Generate new message file for all error states.
# NOTE: This will overwrite existing messages, so use with caution.
.PHONY := create-messages
create-messages:
	@dune exec menhir -- $(parser_file) --list-errors >$(message_file)

# Update auto-generated message file comments for all error states.
.PHONY := update-messages
update-messages:
	@cp -f $(message_file) $(backup_message_file)
	@dune exec menhir -- \
	    $(parser_file) \
	    --update-errors $(message_file) \
	    >$(updated_message_file)
	@mv $(updated_message_file) $(message_file)

# Strip auto-generated comments from message file.
.PHONY := strip-message-comments
strip-message-comments:
	@sed -e "/^##/d" -i.bak $(message_file)
	@rm $(message_file).bak

# Add placeholder messages for unhandled error states to message file.
.PHONY := complete-messages
complete-messages:
	@dune exec menhir -- $(parser_file) --list-errors >$(auto_message_file)
	@dune exec menhir -- \
	    $(parser_file) \
	    --merge-errors $(auto_message_file) \
	    --merge-errors $(message_file) \
	    >$(merged_message_file)
	@mv $(merged_message_file) $(message_file)
