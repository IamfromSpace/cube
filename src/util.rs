extern crate crossbeam;

use std::sync::Mutex;
use self::crossbeam::thread;

pub fn n_scoped_workers<F: Sync + Fn() -> ()>(n: usize, f: F) -> () {
    thread::scope(|s| {
        for _ in 0..n {
            s.spawn(|_| f());
        }
    //TODO: Return the result, rather than unwrapped
    }).unwrap()
}

pub fn while_iter_in_mutex_has_next<I: Iterator, F: Sync + Fn(I::Item) -> ()>(m: &Mutex<I>, f: F) -> () {
    loop {
        // TODO: Propogate the Poisoning
        let mut guard = m.lock().unwrap();
        let perm_o = (*guard).next();
        drop(guard);
        match perm_o {
            // TODO: Allow a return value/result to be collected
            Some(perm) => f(perm),
            None => break,
        };
    }
}
