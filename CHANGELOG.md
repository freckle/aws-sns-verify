## [_Unreleased_](https://github.com/freckle/aws-sns-verify/compare/v0.1.0.0...main)

## [v0.1.0.0](https://github.com/freckle/aws-sns-verify/compare/v0.0.0.3...v0.1.0.0)

- Replace `memory` with `ram` for `crypton-1.1`

- Raise a slew of lower bounds, to accomplish the above

- Drop support for GHCs lower than 9.4, to accomplish the above

- Return more specific errors in URI validation

  Rather than respond `BadUri` for non-URI, unexpected scheme, or invalid domain
  name, we now have 3 distinct error constructors for each of those.

## [v0.0.0.3](https://github.com/freckle/aws-sns-verify/compare/v0.0.0.2...v0.0.0.3)

- Migrate to `crypton-x509*`
- Remove CI for GHC's 8.6 and 8.8

## [v0.0.0.2](https://github.com/freckle/aws-sns-verify/compare/v0.0.0.1...v0.0.0.2)

- Validate PEM has come from AWS before checking signature.

## [v0.0.0.1](https://github.com/freckle/aws-sns-verify/compare/v0.0.0.0...v0.0.0.1)

- Fix typo in subscribe signature

## [v0.0.0.0](https://github.com/freckle/aws-sns-verify/releases/tag/v0.0.0.0)

- Initial release.
- Handle JSON parsing.
- Handle validation of payload signatures via X509.
- Handle response to subscription messages.
