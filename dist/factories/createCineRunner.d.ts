import 'phaser';
import { YarnNode } from 'bondage/types/bondageTypes';
import bondage from 'bondage';
import { ITextBox } from '../components/TextWindow';
import { ILevelComponents } from '../ILevel';
export default function (level: ILevelComponents, yarnjson: YarnNode[], textWindow: ITextBox): bondage.Runner;
