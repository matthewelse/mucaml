#![no_std]
#![no_main]

extern crate alloc;

use alloc::vec;

use panic_semihosting as _;

use cortex_m_rt::entry;
use cortex_m_semihosting::hprintln;

use embedded_alloc::LlffHeap as Heap;

#[global_allocator]
static HEAP: Heap = Heap::empty();

extern "C" {
    fn mucaml_main() -> i32;
}

#[entry]
fn main() -> ! {
    {
        use core::mem::MaybeUninit;
        const HEAP_SIZE: usize = 1024 * 1024;
        static mut HEAP_MEM: [MaybeUninit<u8>; HEAP_SIZE] = [MaybeUninit::uninit(); HEAP_SIZE];
        unsafe { HEAP.init(&raw mut HEAP_MEM as usize, HEAP_SIZE) }
    }

    let mut vec = vec![1, 2, 3, 4];
    vec.push(1000);

    hprintln!("hello, world: {:?}", vec);

    let result = unsafe {
         mucaml_main()
    };

    hprintln!("program returned: {:?}", result);

    loop { }
}
