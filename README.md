elixir_chat
===========

This is an Elixir Chat Server based on Chris Moos's
[Building an Erlang Chat Server with Comet](http://www.chrismoos.com/2009/09/28/building-an-erlang-chat-server-with-comet-part-1)
the source of which can be found on [Github](https://github.com/chrismoos/erl_chat_tutorial).

After porting the original Erlang code we'll then make some changes:

* Allow multiple rooms
* Use Web Sockets instead of Comet

For the web sockets changes, we'll take some of the ideas from Loïc Hoguin's Erlang DC Feb 2013 talk
on Cowboy and Web Sockets [Youtube video](http://www.youtube.com/watch?v=yYlYZy1Jhzk) and [Github code](https://github.com/essen/chat_tutorial).

This tutorial is split into different parts to make it easier to follow along, rather than using git tags or branches.

* Part 1 - The Original Chat Server Backend
* Part 2 - Adding the Web Client
* Part 3 - Adding Multiple Rooms
* Part 4 - Using Web Sockets
