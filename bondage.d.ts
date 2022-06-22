/** Declaration file generated by dts-gen */
declare module 'bondage' {
    export interface TextResult {
        text: string
        lineNum: number
    }

    export interface CommandResult {
        name: string
        args: string[]
        result: any
    }

    export interface OptionsResult {
        options: string[]
        lineNum: number
        selected: number
        select(index: number): void
    }

    export type BondageResults = OptionsResult | TextResult | CommandResult

    export interface YarnNode {
        title: string
        tags: string
        body: string
    }

    interface VariableStorer {
        set(key: string, obj: any): void
        get(key: string): any
    }

    export class Runner {
        constructor();

        public yarnNodes: YarnNode[]

        load(yarnjson: YarnNode[]): void;
        loadYarnString(yarnstring: string): void

        registerFunction(key: string, func: Function): void;

        run(node: string): Generator<BondageResults,BondageResults,undefined>;

        setVariableStorage<T extends VariableStorer>(store:T): void;

    }

}
