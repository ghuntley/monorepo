After having tested countless messaging apps over the years, being
unsatisfied with most of them and finally getting stuck with
[Telegram](https://telegram.org/) I have developed a little theory about
messaging apps.

SMU stands for *Security*, *Multi-Device* and *Usability*. Quite like
the [CAP-theorem](https://en.wikipedia.org/wiki/CAP_theorem) I believe
that you can - using current models - only solve two out of three things
on this list. Let me elaborate what I mean by the individual points:

**Security**: This is mainly about encryption of messages, not so much
about hiding identities to third-parties. Commonly some kind of
asymmetric encryption scheme. Verification of keys used must be possible
for the user.

**Multi-Device**: Messaging-app clients for multiple devices, with
devices being linked to the same identifier, receiving the same messages
and being independent of each other. A nice bonus is also an open
protocol (like Telegram\'s) that would let people write new clients.

**Usability**: Usability is a bit of a broad term, but what I mean by it
here is handling contacts and identities. It should be easy to create
accounts, give contact information to people and have everything just
work in a somewhat automated fashion.

Some categorisation of popular messaging apps:

**SU**: Threema

**MU**: Telegram, Google Hangouts, iMessage, Facebook Messenger

**SM**:
[Signal](https://gist.github.com/TheBlueMatt/d2fcfb78d29faca117f5)

*Side note: The most popular messaging app - WhatsApp - only scores a
single letter (U). This makes it completely uninteresting to me.*

Let\'s talk about **SM** - which might contain the key to solving SMU.
Two approaches are interesting here.

The single key model
--------------------

In Signal there is a single identity key which can be used to register a
device on the server. There exists a process for sharing this identity
key from a primary device to a secondary one, so that the secondary
device can register itself (see the link above for a description).

This *almost* breaks M because there is still a dependence on a primary
device and newly onboarded devices can not be used to onboard further
devices. However, for lack of a better SM example I\'ll give it a pass.

The other thing it obviously breaks is U as the process for setting it
up is annoying and having to rely on the primary device is a SPOF (there
might be a way to recover from a lost primary device, but I didn\'t find
any information so far).

The multiple key model
----------------------

In iMessage every device that a user logs into creates a new key pair
and submits its public key to a per-account key pool. Senders fetch all
available public keys for a recipient and encrypt to all of the keys.

Devices that join can catch up on history by receiving it from other
devices that use its public key.

This *almost* solves all of SMU, but its compliance with S breaks due to
the fact that the key pool is not auditable, and controlled by a
third-party (Apple). How can you verify that they don\'t go and add
another key to your pool?

A possible solution
-------------------

Out of these two approaches I believe the multiple key one looks more
promising. If there was a third-party handling the key pool but in a way
that is verifiable, transparent and auditable that model could be used
to solve SMU.

The technology I have been thinking about for this is some kind of
blockchain model and here\'s how I think it could work:

1.  Bob installs the app and begins onboarding. The first device
    generates its keypair, submits the public key and an account
    creation request.

2.  Bob\'s account is created on the messaging apps\' servers and a
    unique identifier plus the fingerprint of the first device\'s public
    key is written to the chain.

3.  Alice sends a message to Bob, her device asks the messaging service
    for Bob\'s account\'s identity and public keys. Her device verifies
    the public key fingerprint against the one in the blockchain before
    encrypting to it and sending the message.

4.  Bob receives Alice\'s message on his first device.

5.  Bob logs in to his account on a second device. The device generates
    a key pair and sends the public key to the service, the service
    writes it to the blockchain using its identifier.

6.  The messaging service requests that Bob\'s first device signs the
    second device\'s key and triggers a simple confirmation popup.

7.  Bob confirms the second device on his first device. It signs the key
    and writes the signature to the chain.

8.  Alice sends another message, her device requests Bob\'s current keys
    and receives the new key. It verifies that both the messaging
    service and one of Bob\'s older devices have confirmed this key in
    the chain. It encrypts the message to both keys and sends it on.

9.  Bob receives Alice\'s message on both devices.

After this the second device can request conversation history from the
first one to synchronise old messages.

Further devices added to an account can be confirmed by any of the
devices already in the account.

The messaging service could not add new keys for an account on its own
because it does not control any of the private keys confirmed by the
chain.

In case all devices were lost, the messaging service could associate the
account with a fresh identity in the block chain. Message history
synchronisation would of course be impossible.

Feedback welcome
----------------

I would love to hear some input on this idea, especially if anyone knows
of an attempt to implement a similar model already. Possible attack
vectors would also be really interesting.

Until something like this comes to fruition, I\'ll continue using
Telegram with GPG as the security layer when needed.

**Update:** WhatsApp has launched an integration with the Signal guys
and added their protocol to the official WhatsApp app. This means
WhatsApp now firmly sits in the SU-category, but it still does not solve
this problem.

**Update 2:** Facebook Messenger has also integrated with Signal, but
their secret chats do not support multi-device well (it is Signal
afterall). This means it scores either SU or MU depending on which mode
you use it in.

An interesting service I have not yet evaluated properly is
[Matrix](http://matrix.org/).
