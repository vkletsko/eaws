PROJECT = eaws
PROJECT_DESCRIPTION = Erlang Library to send mail using Amazon Web Services.
PROJECT_VERSION = v0.3.0

# Options.
CI_OTP ?= OTP-18.0.3 OTP-18.1.5 OTP-18.2.4.1 OTP-18.3.4.4 OTP-19.0.7 OTP-19.1.6 OTP-19.2

# Standard targets.

include erlang.mk

# Compile options.

ERLC_OPTS += +warn_export_all +warn_missing_spec +warn_untyped_record
TEST_ERLC_OPTS += +'{parse_transform, eunit_autoexport}'

DIALYZER_OPTS += -Wunmatched_returns
PLT_APPS = stdlib crypto public_key ssl

# Generate rebar.config on build.

app:: rebar.config

