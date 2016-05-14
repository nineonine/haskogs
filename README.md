Haskogs
---

A Haskell Library for interacting with [Discogs API](https://www.discogs.com/developers/).

### Features
+ Covers all Endpoints
+ Supports pagination, optional parameters
+ Can be easily embedded in your monad transformer stack
+ Includes test suites

### Examples

##### Database

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Discogs.Types.Discogs
import Discogs.Types.Release
import Discogs.Types.Artist

token :: ByteString
token = "yourGeneratedToken"

program :: DiscogsT IO (Release, Artist)
program = do
    r <- release 1
    a <- artist 1
    return (r, a)

main :: IO ()
main = do
    (r , a) <- fmap fromRightMaybe $ runDiscogs token program
    print r
    print a

```

##### Marketplace

```haskell  
{-# LANGUAGE OverloadedStrings #-}
import Discogs.Types.Discogs
import Discogs.Types.MarketPlace.Listing

token :: ByteString
token = "yourGeneratedToken"

program :: DiscogsT IO ()
program = newListing 4941528 "Mint (M)" 100 "Draft" Nothing >> return ()

main :: IO ()
main = runDiscogs token program >> return ()

```

##### User Actions

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Discogs.Types.Discogs
import Discogs.Types.User

token :: ByteString
token = "yourGeneratedToken"

program :: DiscogsT IO ()
program = profile "username"

main :: IO ()
main = do
    Right ( Just prfl ) <- runDiscogs token program
    print prfl

```

### Optional Parameters

Some endpoints accept optional parameters.
You can pass them using special type Params.
It is wrapped in existential type, so you just have to pass `Text` and `Params` will take care of conversion.

Example

```haskell
-- without optional params
runDiscogs token $ newListing 4941528 "Mint (M)" 100 "Draft" Nothing

-- with optional params
runDiscogs token $ newListing 4941528 "Mint (M)" 100 "Draft" (Just [("location", "unknown")])

```

### Testing

2 test suites available

`cabal test test`

for testing JSON instances

`cabal test test-io`

for testing IO.

### TODO
+ OAuth Authentication
+ Richer Error Types
+ Automatic pausing when request limit reached
+ More Examples
+ Finish Documentation

### Contributions
+ Fork this repo
+ Create a new branch on your fork
+ Push commits to yr branch
+ Submit new Pull Request


### License
MIT License 2016 © Alex Dudarenko
