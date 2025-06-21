#![no_std]
#![no_main]

use panic_halt as _;

use rp235x_hal as hal;

use embedded_hal::delay::DelayNs;
use embedded_hal::digital::OutputPin;

#[unsafe(link_section = ".start_block")]
#[used]
pub static IMAGE_DEF: hal::block::ImageDef = hal::block::ImageDef::secure_exe();

const XTAL_FREQ_HZ: u32 = 12_000_000u32;

extern "C" {
    fn mucaml_main() -> u32;
}

#[no_mangle]
pub extern "C" fn mucaml_sleep_ms(ms: u32) {
    let mut pac = unsafe { hal::pac::Peripherals::steal() };
    let mut watchdog = hal::Watchdog::new(pac.WATCHDOG);
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
    let mut timer = hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);
    timer.delay_ms(ms);
}

#[no_mangle]
pub extern "C" fn mucaml_led_on(_: u32) {
    let mut pac = unsafe { hal::pac::Peripherals::steal() };
    // The single-cycle I/O block controls our GPIO pins
    let sio = hal::Sio::new(pac.SIO);

    // Set the pins to their default state
    let pins = hal::gpio::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );
    let mut led_pin = pins.gpio7.into_push_pull_output();
    led_pin.set_high().unwrap();
}

#[no_mangle]
pub extern "C" fn mucaml_led_off(_: u32) {
    let mut pac = unsafe { hal::pac::Peripherals::steal() };
    // The single-cycle I/O block controls our GPIO pins
    let sio = hal::Sio::new(pac.SIO);

    // Set the pins to their default state
    let pins = hal::gpio::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );
    let mut led_pin = pins.gpio7.into_push_pull_output();
    led_pin.set_low().unwrap();
}

#[hal::entry]
fn main() -> ! {
    let result = unsafe { mucaml_main() };

    // Grab our singleton objects
    let mut pac = unsafe { hal::pac::Peripherals::steal() };

    // Set up the watchdog driver - needed by the clock setup code
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

    let mut timer = hal::Timer::new_timer0(pac.TIMER0, &mut pac.RESETS, &clocks);

    timer.delay_ms(1000);

    // The single-cycle I/O block controls our GPIO pins
    let sio = hal::Sio::new(pac.SIO);

    // Set the pins to their default state
    let pins = hal::gpio::Pins::new(
        pac.IO_BANK0,
        pac.PADS_BANK0,
        sio.gpio_bank0,
        &mut pac.RESETS,
    );

    let mut led_pin = pins.gpio7.into_push_pull_output();

    for _i in 0..result {
        led_pin.set_high().unwrap();
        timer.delay_ms(500);
        led_pin.set_low().unwrap();
        timer.delay_ms(500);
    }

    loop {}
}
