import { InjectionToken } from '@angular/core';

import { Observable }                                        from 'rxjs';

export const BASE_PATH = new InjectionToken<string>('basePath');
export const BASE_PATH_OBSERVABLE = new InjectionToken<Observable<string>>('basePathObservable');
export const COLLECTION_FORMATS = {
    'csv': ',',
    'tsv': '   ',
    'ssv': ' ',
    'pipes': '|'
}
