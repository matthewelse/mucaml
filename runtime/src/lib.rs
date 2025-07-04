#![no_std]

extern crate alloc;

// #[cfg(target_arch = "arm")]
// use panic_semihosting as _;

use panic_halt as _;

#[cfg(target_arch = "arm")]
use cortex_m_rt as _;

#[cfg(not(target_arch = "arm"))]
use core::panic::PanicInfo;

#[cfg(not(target_arch = "arm"))]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[no_mangle]
pub fn __aeabi_unwind_cpp_pr0() {
}

#[cfg(target_arch = "arm")]
#[no_mangle]
pub extern "C" fn mucaml_print(n: i32) {
    use core::fmt::Write;
    use cortex_m_semihosting::hio;

    let mut stdout = hio::hstdout().unwrap();
    writeln!(stdout, "mucaml_print: {}", n).unwrap();
}

#[cfg(feature = "alloc")]
use embedded_alloc::LlffHeap as Heap;

#[cfg(feature = "alloc")]
#[global_allocator]
static HEAP: Heap = Heap::empty();

    const XTAL_FREQ_HZ: u32 = 12_000_000u32;
    use alloc::boxed::Box;
    use embedded_hal::delay::DelayNs;
    use rp235x_hal::gpio::{FunctionSio, Pin, SioOutput};
    use rp235x_hal as hal;
    use rp235x_hal::timer::CopyableTimer0;
    use embedded_hal::digital::OutputPin;

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_pac(_: i32) -> &'static mut hal::pac::Peripherals {
        // Grab our singleton objects
        Box::leak(Box::new(hal::pac::Peripherals::take().unwrap()))
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_pins(
        mut pac: Box<hal::pac::Peripherals>,
    ) -> &'static mut hal::gpio::Pins {
        // The single-cycle I/O block controls our GPIO pins
        let sio = hal::Sio::new(pac.SIO);

        // Set the pins to their default state
        let pins = hal::gpio::Pins::new(
            pac.IO_BANK0,
            pac.PADS_BANK0,
            sio.gpio_bank0,
            &mut pac.RESETS,
        );

        Box::leak(Box::new(pins))
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_timer(
        mut pac: Box<hal::pac::Peripherals>,
    ) -> &'static mut hal::Timer<CopyableTimer0> {
        let mut watchdog = hal::Watchdog::new(pac.WATCHDOG);

        // Configure the clocks
        let clocks = hal::clocks::init_clocks_and_plls(
            XTAL_FREQ_HZ,
            pac.XOSC,
            pac.CLOCKS,
            pac.PLL_SYS,
            pac.PLL_USB,
            &mut pac.RESETS,
            &mut watchdog,
        )
        .unwrap();

        let timer = hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);

        Box::leak(Box::new(timer))
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_timer_sleep_500ms(mut timer : Box<hal::Timer<CopyableTimer0>>) {
        timer.delay_ms(500);
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_pin(pins : Box<rp235x_hal::gpio::Pins>) -> &'static mut Pin<hal::gpio::bank0::Gpio7, FunctionSio<SioOutput>, hal::gpio::PullDown> {
        Box::leak(Box::new(pins.gpio7.into_push_pull_output()))
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_pin_set_high(mut pin : Box<Pin<hal::gpio::bank0::Gpio7, FunctionSio<SioOutput>, hal::gpio::PullDown>>) -> () {
        pin.set_high().unwrap();
    }

    #[no_mangle]
    pub extern "C" fn mucaml_rpi_pin_set_low(mut pin : Box<Pin<hal::gpio::bank0::Gpio7, FunctionSio<SioOutput>, hal::gpio::PullDown>>) -> () {
        pin.set_low().unwrap();
    }

#[cfg(target_arch = "arm")]
#[no_mangle]
pub extern "C" fn mucaml_exit(n: i32) {
    use core::fmt::Write;
    use cortex_m_semihosting::{debug, hio};

    let mut stdout = hio::hstdout().unwrap();
    writeln!(stdout, "mucaml_exit: {}", n).unwrap();
    debug::exit(debug::EXIT_SUCCESS);
}

#[cfg(not(target_arch = "arm"))]
#[no_mangle]
pub extern "C" fn mucaml_print(n: i32) {
    println!("mucaml_print: {}", n);
}

extern "C" {
    fn mucaml_main(_: i32) -> i32;
}

#[rp235x_hal::entry]
fn main() -> ! {
    if cfg!(feature = "alloc") {
        use core::mem::MaybeUninit;
        const HEAP_SIZE: usize = 1024;
        static mut HEAP_MEM: [MaybeUninit<u8>; HEAP_SIZE] = [MaybeUninit::uninit(); HEAP_SIZE];
        unsafe { HEAP.init(&raw mut HEAP_MEM as usize, HEAP_SIZE) }
    }

    let result = unsafe { mucaml_main(0) };

    mucaml_print(result);

    loop {}
}

