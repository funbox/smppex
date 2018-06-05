# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Handling of generic `GenServer` `call` and `cast` messages.

### Changed
- Fixed handling of PDUs with negative `send_pde_result` status: they do not appear 
in `handle_resp_timeout` callack anymore.

## [2.2.5] - 2018-05-23

### Added
- `submit_sm` factory methods with automatic TON/NPI detection.

### Changed
- Dropped Elixir 1.1.1 and OTP 17 support.
- Made `SMPPEX.ESME.Sync` be safe for making requestd from multiple processes.

## [2.2.4] - 2018-02-12

### Changed
- Updated build matrix for Travis CI. Removed assets for obsolete version builds.
- Updated build matrix for Travis CI. Added Elixir 1.6.
- Added explicit extract functions in SMPPEX.Pdu.Multipart.
- Fixed Ranch transport handling in SMPPEX.TransportSession, this fixes SSL transport support.

## [2.2.3] - 2017-09-21

### Changed
- SMPPEX.ESME.Sync: ignore successful send_pdu_result for syncronously sent PDUs.
- SMPPEX.ESME.Sync: exit with normal on socket close.

## [2.2.2] - 2017-09-21
### Changed
- Fixed handling socket close/error for SMPPEX.ESME.Sync

## [2.2.1] - 2017-09-11
### Changed
- Loosened `ranch` version requirements.

## [2.2.0] - 2017-09-10
### Changed
- Added strict response type for `terminate` callback. Also, `terminate` callback is now allowed to return some last pdus for sending.

## [2.1.0] - 2017-09-10
### Added
- Automatic handling of `enquire_link` and `enquire_link_resp` PDUs.

## [2.0.1] - 2017-09-02
### Changed
- `PduStorage` implementation to be OTP 17 compatible.

## [2.0.0] - 2017-08-30
This release contains significant architectural changes and API incompatibilities.

### Changed
- Added single `SMPPEX.Session` behavior instead of seperate `SMPPEX.ESME` and `SMPPEX.MC` behaviours.
- `SMPPEX.Session` callbacks are more `GenServer` compliant.
- Most of `SMPPEX.Session` callbacks are allowed to return stop indicating tuple.
- Most of `SMPPEX.Session` callbacks are allowed to return a list of PDUs to send.
- All methods interacting with `SMPPEX.Session` are synchronous.

### Added
- Elixir 1.5 builds in CI.

### Removed
- Usage of a separate process for ESME/MC connections.
- Usage of `ranch` supervisor of launching ESME connections.
- Special methods for sending PDU replies to ESME/MC in favor of `Pdu.as_reply_to/2` method.

## [1.0.1] - 2017-08-30
### Changed
- `ex_doc` updated to 1.x.


## [1.0.0] - 2017-08-30
This is the first stable release introduced from numerous 0.x.x versions. Although this library has been already used in production for quite a while, it's only purpose is to designate the divergence from 2.x.x branch.

### Added
- ESME functionality.
- MC functionality.
- Synchronous ESME client.
