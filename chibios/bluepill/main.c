/*
    ChibiOS - Copyright (C) 2006..2018 Giovanni Di Sirio

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/

#include <stdio.h>
#include <string.h>

#include "ch.h"
#include "hal.h"

#include "chprintf.h"
#include "shell.h"

#include "usbcfg.h"

/*===========================================================================*/
/* Command line related.                                                     */
/*===========================================================================*/

#define SHELL_WA_SIZE   THD_WORKING_AREA_SIZE(2048)

static const ShellCommand commands[] = {
  {NULL, NULL}
};

static const ShellConfig shell_cfg1 = {
  (BaseSequentialStream *)&SDU1,
  commands
};

/*===========================================================================*/
/* Main and generic code.                                                    */
/*===========================================================================*/

static thread_t *shelltp = NULL;



/*
 * Shell exit event.
 */
static void ShellHandler(eventid_t id) {

  (void)id;
  if (chThdTerminatedX(shelltp)) {
    chThdWait(shelltp);                 /* Returning memory to heap.        */
    shelltp = NULL;
  }
}

/*
 * Green LED blinker thread, times are in milliseconds.
 */
static THD_WORKING_AREA(waThread1, 128);
static THD_FUNCTION(Thread1, arg) {

  (void)arg;
  chRegSetThreadName("blinker");
  while (true) {
    palTogglePad(GPIOF, GPIOF_LED1);
    chThdSleepMilliseconds(500);
  }
}

/*
 * Application entry point.
 */
int main(void) {
  static const evhandler_t evhndl[] = {
    ShellHandler
  };
  event_listener_t el2;

  /*
   * System initializations.
   * - HAL initialization, this also initializes the configured device drivers
   *   and performs the board-specific initializations.
   * - Kernel initialization, the main() function becomes a thread and the
   *   RTOS is active.
   */
  halInit();
  chSysInit();

  /*
   * Initializes a serial-over-USB CDC driver.
   */
  sduObjectInit(&SDU1);
  sduStart(&SDU1, &serusbcfg);

  /*
   * Activates the USB driver and then the USB bus pull-up on D+.
   * Note, a delay is inserted in order to not have to disconnect the cable
   * after a reset.
   */
  //usbDisconnectBus(serusbcfg.usbp);
  //chThdSleepMilliseconds(1500);
  usbStart(serusbcfg.usbp, &usbcfg);
  //usbConnectBus(serusbcfg.usbp);

  /*
   * Shell manager initialization.
   */
  shellInit();

  /*
   * Creates the blinker thread.
   */
  chThdCreateStatic(waThread1, sizeof(waThread1), NORMALPRIO, Thread1, NULL);

  /*
   * Normal main() thread activity, handling SD card events and shell
   * start/exit.
   */
  chEvtRegister(&shell_terminated, &el2, 2);
  while (true) {
    if (!shelltp && (SDU1.config->usbp->state == USB_ACTIVE)) {
      shelltp = chThdCreateFromHeap(NULL, SHELL_WA_SIZE,
                                    "shell", NORMALPRIO + 1,
                                    shellThread, (void *)&shell_cfg1);
    }
    chEvtDispatch(evhndl, chEvtWaitOneTimeout(ALL_EVENTS, TIME_MS2I(500)));
  }
}
