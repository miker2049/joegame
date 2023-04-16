import { assign, createMachine, sendTo } from 'xstate'
import { MachineRegistry } from './MachineRegistry'

export const createConvoMachine = (
  id: string,
  convo: [string, string][],
  registry: MachineRegistry
) =>
  createMachine(
    {
      id,
      initial: 'lull',
      context: {
        convo,
        registry,
        counter: 0,
        lullLength: 1000
      },
      states: {
        lull: {
          entry: 'incrementCounter',
          after: {
            LULL_LENGTH: [
              { target: 'speaking', cond: 'convoUnfinished' },
              { target: 'paused' }
            ]
          },
          on: {
            STOP: {
              target: 'paused'
            }
          }
        },
        speaking: {
          entry: 'speak',
          on: {
            FINISHED_TALKING: {
              target: 'lull'
            },
            STOP: {
              target: 'paused'
            }
          }
        },
        paused: {
          on: {
            START: {
              target: 'lull',
              actions: 'resetCounter'
            }
          }
        }
      }
    },
    {
      actions: {
        incrementCounter: assign({
          counter: (context) => context.counter + 1
        }),
        resetCounter: assign({
          counter: 0
        }),
        speak: (context) =>
          context.registry.sendTo('npc_' + context.convo[context.counter][1], {
            type: 'SPEAK_THOUGHT',
            text: context.convo[context.counter][0],
            convoId: id
          })
      },
      guards: {
        convoUnfinished: (context) => context.counter < context.convo.length
      },
      delays: {
        LULL_LENGTH: (context) => context.lullLength
      }
    }
  )
