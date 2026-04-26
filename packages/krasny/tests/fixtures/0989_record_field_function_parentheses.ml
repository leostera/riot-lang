let receive_any timeout=Effect.perform(Proc_effect.Receive{selector=(fun msg->`select msg);timeout})
