# distance-of-time

This is a Haskell package capturing readable distances between times.

```haskell
import qualified Data.Time as T
import qualified Data.Time.Distance as TD

-- Given the following signatures
--
-- now :: T.UTCTime
-- publishedOn :: T.UTCTime

TD.distanceOfTimeInWords publishedOn now
```

## Hackage

This package is available on [Hackage].

[Hackage]: http://hackage.haskell.org/package/distance-of-time

## License

DistanceOfTime is Copyright © 2017 Josh Clayton and thoughtbot, inc.. It is
free software, and may be redistributed under the terms specified in the
[LICENSE](/LICENSE) file.

## About thoughtbot

![thoughtbot](https://thoughtbot.com/logo.png)

DistanceOfTime is maintained and funded by thoughtbot, inc.
The names and logos for thoughtbot are trademarks of thoughtbot, inc.

We love open source software and Haskell. See [our other Haskell
projects][haskell-services], or [hire our Haskell development team][hire]
to design, develop, and grow your product.

[haskell-services]: https://thoughtbot.com/services/haskell?utm_source=github
[hire]: https://thoughtbot.com?utm_source=github
