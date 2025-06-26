#![no_std]

#[cfg(target_arch = "arm")]
use panic_semihosting as _;

#[cfg(target_arch = "arm")]
use cortex_m_rt as _;

#[cfg(not(target_arch = "arm"))]
use core::panic::PanicInfo;

#[cfg(not(target_arch = "arm"))]
#[panic_handler]
fn panic(_info: &PanicInfo) -> ! {
    loop {}
}

#[cfg(target_arch = "arm")]
#[no_mangle]
pub extern "C" fn mucaml_print(n : i32) {
    use cortex_m_semihosting::hio;
    use core::fmt::Write;

    let mut stdout = hio::hstdout().unwrap();
    writeln!(stdout, "mucaml_print: {}", n).unwrap();
}

#[cfg(not(target_arch = "arm"))]
#[no_mangle]
pub extern "C" fn mucaml_print(n : i32) {
    println!("mucaml_print: {}", n);
}

extern "C" {
    fn mucaml_main(i32) -> i32;
}

#[no_mangle]
pub fn main() -> ! {
    let result = unsafe { mucaml_main(0) };

    mucaml_print(result);

    loop {}
}

