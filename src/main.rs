extern crate clap;
extern crate posix_mq;
extern crate libc;
extern crate nix;

use clap::{App, SubCommand, Arg, ArgMatches, AppSettings};
use posix_mq::{Name, Queue, Message};
use std::fs::{read_dir, File};
use std::io::{self, Read, Write};
use std::process::exit;

fn run_ls() {
    let mqueues = read_dir("/dev/mqueue")
        .expect("Could not read message queues");

    for queue in mqueues {
        let path = queue.unwrap().path();
        let status = {
            let mut file = File::open(&path)
                .expect("Could not open queue file");

            let mut content = String::new();
            file.read_to_string(&mut content).expect("Could not read queue file");

            content
        };

        let queue_name = path.components().last().unwrap().as_os_str();
        println!("{:?}: {}", queue_name, status)
    };
}

fn run_inspect(queue_name: &str) {
    let name = Name::new(queue_name).expect("Invalid queue name");
    let queue = Queue::open(name).expect("Could not open queue");

    println!("Queue {}:\n", queue_name);
    println!("Max. message size: {} bytes", queue.max_size());
    println!("Max. # of pending messages: {}", queue.max_pending());
}

fn run_create(cmd: &ArgMatches) {
    if let Some(rlimit) = cmd.value_of("rlimit") {
        set_rlimit(rlimit.parse().expect("Invalid rlimit value"));
    }

    let name = Name::new(cmd.value_of("queue").unwrap())
        .expect("Invalid queue name");

    let max_pending: i64 = cmd.value_of("max-pending").unwrap().parse().unwrap();
    let max_size: i64 = cmd.value_of("max-size").unwrap().parse().unwrap();

    let queue = Queue::create(name, max_pending, max_size * 1024);

    match queue {
        Ok(_)  => println!("Queue created successfully"),
        Err(e) => {
            writeln!(io::stderr(), "Could not create queue: {}", e).ok();
            exit(1);
        },
    };
}

fn run_receive(queue_name: &str) {
    let name = Name::new(queue_name).expect("Invalid queue name");
    let queue = Queue::open(name).expect("Could not open queue");

    let message = match queue.receive() {
        Ok(msg) => msg,
        Err(e) => {
            writeln!(io::stderr(), "Failed to receive message: {}", e).ok();
            exit(1);
        }
    };

    // Attempt to write the message out as a string, but write out raw bytes if it turns out to not
    // be UTF-8 encoded data.
    match String::from_utf8(message.data.clone()) {
        Ok(string) => println!("{}", string),
        Err(_) => {
            writeln!(io::stderr(), "Message not UTF-8 encoded!").ok();
            io::stdout().write(message.data.as_ref()).ok();
        }
    };
}

fn run_send(queue_name: &str, content: &str) {
    let name = Name::new(queue_name).expect("Invalid queue name");
    let queue = Queue::open(name).expect("Could not open queue");

    let message = Message {
        data: content.as_bytes().to_vec(),
        priority: 0,
    };

    match queue.send(&message) {
        Ok(_) => (),
        Err(e) => {
            writeln!(io::stderr(), "Could not send message: {}", e).ok();
            exit(1);
        }
    }
}

fn run_rlimit() {
    let mut rlimit = libc::rlimit {
        rlim_cur: 0,
        rlim_max: 0,
    };

    let mut errno = 0;
    unsafe {
        let res = libc::getrlimit(libc::RLIMIT_MSGQUEUE, &mut rlimit);
        if res != 0 {
            errno = nix::errno::errno();
        }
    };

    if errno != 0 {
        writeln!(io::stderr(), "Could not get message queue rlimit: {}", errno).ok();
    } else {
        println!("Message queue rlimit:");
        println!("Current limit: {}", rlimit.rlim_cur);
        println!("Maximum limit: {}", rlimit.rlim_max);
    }
}

fn set_rlimit(new_limit: u64) {
    let rlimit = libc::rlimit {
        rlim_cur: new_limit,
        rlim_max: new_limit,
    };

    let mut errno: i32 = 0;
    unsafe {
        let res = libc::setrlimit(libc::RLIMIT_MSGQUEUE, &rlimit);
        if res != 0 {
            errno = nix::errno::errno();
        }
    }

    match errno {
        0 => println!("Set RLIMIT_MSGQUEUE hard limit to {}", new_limit),
        _ => {
            // Not mapping these error codes to messages for now, the user can
            // look up the meaning in setrlimit(2).
            panic!("Could not set hard limit: {}", errno);
        }
    };
}

fn main() {
    let ls = SubCommand::with_name("ls").about("list message queues");

    let queue_arg = Arg::with_name("queue").required(true).takes_value(true);

    let rlimit_arg = Arg::with_name("rlimit")
        .help("RLIMIT_MSGQUEUE to set for this command")
        .long("rlimit")
        .takes_value(true);

    let inspect = SubCommand::with_name("inspect")
        .about("inspect details about a queue")
        .arg(&queue_arg);

    let create = SubCommand::with_name("create")
        .about("Create a new queue")
        .arg(&queue_arg)
        .arg(&rlimit_arg)
        .arg(Arg::with_name("max-size")
            .help("maximum message size (in kB)")
            .long("max-size")
            .required(true)
            .takes_value(true))
        .arg(Arg::with_name("max-pending")
            .help("maximum # of pending messages")
            .long("max-pending")
            .required(true)
            .takes_value(true));

    let receive = SubCommand::with_name("receive")
        .about("Receive a message from a queue")
        .arg(&queue_arg);

    let send = SubCommand::with_name("send")
        .about("Send a message to a queue")
        .arg(&queue_arg)
        .arg(Arg::with_name("message")
            .help("the message to send")
            .required(true));

    let rlimit = SubCommand::with_name("rlimit")
        .about("Get the message queue rlimit")
        .setting(AppSettings::SubcommandRequiredElseHelp);

    let matches = App::new("mq")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version("0.0.1")
        .about("Administrate and inspect POSIX message queues")
        .subcommand(ls)
        .subcommand(inspect)
        .subcommand(create)
        .subcommand(receive)
        .subcommand(send)
        .subcommand(rlimit)
        .get_matches();

    match matches.subcommand() {
        ("ls", _) => run_ls(),
        ("inspect", Some(cmd)) => run_inspect(cmd.value_of("queue").unwrap()),
        ("create",  Some(cmd)) => run_create(cmd),
        ("receive", Some(cmd)) => run_receive(cmd.value_of("queue").unwrap()),
        ("send",    Some(cmd)) => run_send(
            cmd.value_of("queue").unwrap(),
            cmd.value_of("message").unwrap()
        ),
        ("rlimit",  _) => run_rlimit(),
        _ => unimplemented!(),
    }
}
