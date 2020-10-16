# Chusky
## What's the purpose of Chusky?
Well, honestly, I don't know. gRPC, GraphQL and Thrift. I don't like them for one very specific reason.
There are not input/ouput GADT support. Specially for tagged union types. And that's very very annoying.


The purpose of this code would be very fucking simple then:
 Allow for definition of rpc services where we can use GADTs as building blocks of our definitions.
