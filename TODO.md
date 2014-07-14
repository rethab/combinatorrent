The TODO list
=============

(This is a Markdown file)

The list of things that needs done. Feel free to take anything on the
list if you want, but do coordinate so we don't do multiple work on
the same thing. Feel free to add anything to the list as well. It
partially acts like a bug tracker at the moment in addition to being a
wish-list.

   - Handle error cases when checking a torrent file.
   - Do not connect to ourselves (solved if peers arrive from tracker
     as dict, but there's the binary case, where only ip and port is known)
   - (thomaschrstnsn) Implement a creator for torrent files
   - If we get a wrong URI, the code currently deadlocks since the tracker
     dies. Handle this problem gracefully.
   - When we grab pieces from the Piece Manager, let it provide us with a
     pruned set of pieces we can ask with later. This way, we only need to
     consider pieces we already have once and we get a faster system.
     When doing this, only prune pieces which are done and checked.
   - Consider letting the supervisors support monitoring of processes. Use
     this to reimplement parts of the PeerMgr code.
   - Improve synchronization when the supervisor tree is closing down.
     Currently the problem is that the supervisor tree will close down by
     asynchronous messages, so the sync on stopping tree will not wait until
     the subtree is done. This has another quite dangerous implication:
     Stray indefinite blocks on mvars when closing down.
     The fix is to build more structure into the closing of the supervisor
     tree and make it properly synchronous.
   - Cut down communication from the Receiver to the Peer Control process:
     When the Receiver Process runs, it should try to drain its socket as
     much as possible before trying to communicate with the peer. It should
     also try to drain the socket again while waiting on the Control
     process. Doing this will lower the contended point of communication in
     the system.
   - Should the PieceManager know about the InfoHash? We could run a version
     without this knowledge.

Items for later (no particular order)
-------------------------------------

   - Reduce CPU load. Alternative representations of various data structures
     are needed.
   - Add prioritization support of multiTorrents
   - Design, build and improve a graphic UI.
   - Design, build and improve a protocol for communicating with the client.
   - Azureus/Vuze has a keepalive flood detector built-in. Consider if this
     is relevant for this client.
   - Process monitoring in general. Think.
   - Write a fuzzing framework for bittorrent.

# vim: filetype=none tw=76 expandtab
