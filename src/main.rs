extern crate clap;
extern crate posix_mq;

use clap::{App, SubCommand, Arg, AppSettings};
use posix_mq::{Name, Queue};

fn run_ls() {
    use std::fs::{read_dir, File};
    use std::io::Read;

    let mqueues = read_dir("/dev/mqueue")
        .expect("Could not read message queues");

    mqueues.for_each(|queue| {
        //let queue_name = (&queue.unwrap().file_name()).into_string().unwrap();
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
    });
}

fn run_inspect(queue_name: &str) {
    let name = Name::new(queue_name).expect("Invalid queue name");
    let queue = Queue::open(name).expect("Could not open queue");

    println!("Queue {}:\n", queue_name);
    println!("Max. message size: {} bytes", queue.max_size());
    println!("Max. # of pending messages: {}", queue.max_pending());
}

fn main() {
    let ls = SubCommand::with_name("ls").about("list message queues");
    let inspect = SubCommand::with_name("inspect")
        .about("inspect details about a queue")
        .arg(Arg::with_name("queue")
            .short("q")
            .required(true)
            .takes_value(true));


    let matches = App::new("mq")
        .setting(AppSettings::SubcommandRequiredElseHelp)
        .version("0.0.1")
        .about("Administrate and inspect POSIX message queues")
        .subcommand(ls)
        .subcommand(inspect)
        .get_matches();

    match matches.subcommand() {
        ("ls", _) => run_ls(),
        ("inspect", Some(cmd)) => run_inspect(cmd.value_of("queue").unwrap()),
        _ => unimplemented!(),
    }
}
