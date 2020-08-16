-- A "condition machine" scheduler.
--
-- Not quite sure yet if this is meaningful, but here is the idea: On
-- a uC, it is very convenient to work with round robin polling tasks.
-- E.g. instead of using an explicit scheduler, resume each task and
-- let it inspect its environment and decide if it is ready to
-- continue.
--
-- What I want to do is to invert this.  How can these conditions be
-- made explicit, e.g. converted to some kind of eventing mechanism
-- like channels or mailboxes, without getting bogged down in details.
-- I'm clearly not the first one to prefer this approach, as the
-- existence of condition variables indicates.
--
-- The difference between events is that we only care about changes
-- from the perspective of the task.  E.g. it can ignore all events
-- that it would otherwise be interested in, and only look for value
-- changes.  This difference seems fundamental, and reminds me of FRP.
--
-- https://en.wikipedia.org/wiki/Monitor_(synchronization)#Condition_variables
--
-- I'm currently only interested in cooperative multitasking, so
-- mutual exclusion is implicit.  It seems that the only thing that is
-- necessary is to abstract the condition somehow, and create a
-- derivative, e.g. translate its change into an event.
--
-- So how to do that?





