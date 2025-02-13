class VoiceLanguages:
    arab_set = {'арабский', 'العربية'}
    chinese_set = {'китайский упрощённый', 'китайский(упр.)', 'китайский (упр.)', '中文(简体)', '中文(繁體)', 'chinese'}
    spanish_set = {'español', 'испанский', 'испанский (2)', 'испанский (+ латиноамериканский)', 'spanish'}
    portuguese_set = {'бразильский португальский', 'португальский (+ бразильский)', 'portuguese – brazil', 'português',
                      'português do brasil', 'португальский бразильский', 'бр. португальский'}
    german_set = {'german', 'немецкий', 'deutsch', 'deutsch'}
    russian_set = {'русский (в dlc отсутствует)', 'русский', 'русский (only!)', 'русский|английский',
                   'русский (mass effect 1)', 'английский + русификатор', 'русификатор', 'русский + русификатор',
                   'русский (dlc не все озвучены на русском)'}
    english_set = {'английский\\немецкий', 'english', 'eng', 'английский', 'english + русификаторы',
                   'английский\\японский', 'english |multi', 'английский + русский (отдельной установкой)',
                   'английский | японский'}
    french_set = {'французский', 'français', 'french'}
    italian_set = {'итальянский', 'italiano', 'italian'}
    japanese_set = {'японский', 'японский + отдельный русификатор на базе исходной локализации от «1с»', 'japanese',
                    '日本語'}
    korean_set = {'корейский', 'korean', '한국어'}
    polish_set = {'польский', 'polski', 'polish'}
    ukrainian_set = {'украинский', 'українська'}
    czech_set = {'чешский', 'český'}
    turkish_set = {'турецкий', 'türkçe'}
    hungarian_set = {'magyar'}
    thai_set = {'тайский'}
    danish_set = {'датский'}
    no_translation_set = {'отсутствует (не требуется)', 'никакой', 'нет', 'отсутствует', 'непереводимый', '-', '-||-'}

    by_name = {
        'arab': arab_set,
        'chinese': chinese_set,
        'spanish': spanish_set,
        'portuguese': portuguese_set,
        'german': german_set,
        'russian': russian_set,
        'english': english_set,
        'french': french_set,
        'italian': italian_set,
        'japanese': japanese_set,
        'korean': korean_set,
        'polish': polish_set,
        'ukrainian': ukrainian_set,
        'czech': czech_set,
        'turkish': turkish_set,
        'hungarian': hungarian_set,
        'thai': thai_set,
        'danish': danish_set,
        'no_translation': no_translation_set,
    }


genres_set = {'2021 года', '2018 года', '2012 года', '2016 года', 'action', 'simulation', 'arcade', '2013 года',
              'лицензии', 'fighting', 'vr игры', '2014 года', 'horror', '2020 года', '2015 года', '2023 года',
              '1st person', '2022 года', 'торрент игры', '3d', '2024 года', '2017 года', 'gog', 'strategy',
              'stealth', 'adventure', 'оффлайн', 'racing', 'simulator', 'rpg', '3rd person', 'real-time',
              '2019 года', 'quest', 'shooter', 'decepticon'}


class TextLanguages:
    arab_set = {'арабский', 'العربية'}
    chinese_set = {'китайский (трад.)', 'китайский (упрощённый)', 'китайский (оба)', 'китайский (два варианта)',
                   'китайский (2)', 'китайский простой', 'китайский (простой)', 'китайский (упр.)',
                   'китайский (трад.) + русификатор', 'традиционный китайский', '中文(简体)', '中文(繁體)', '中文',
                   'китайский (2 варианта)', 'китайский упрощённый'}
    spanish_set = {'испанский (+ латиноамериканский)', 'испанский', 'испанский (2)', 'испанский + мод-русификатор',
                   'испанский латиноамериканский', 'español (al)', 'español', 'испанский - испания'}
    portuguese_set = {'português do brasil', 'португальский (+ бразильский)', 'португальский (2)',
                      'португальский (бр.)', 'португальский', 'португальский бразильский', 'бразильский португальский',
                      'португальский - бразилия', 'português'}
    german_set = {'немецкий + русификатор', 'немецкий', 'deutsch', 'deutsch'}
    russian_set = {'русский (russian only!)', 'русский (only!)', 'русский (отдельно', 'русский с модом expansion',
                   'русский|английский', 'rus|eng', 'русский', 'русский (в dlc отсутствует)', 'pусский',
                   'русский через mod.io в настройках меню', 'русский + русификатор',
                   'русский; английский; немецкий; французский; итальянский; испанский; португальский; польский; '
                   'турецкий; японский; корейский; китайский упрощённый'}
    english_set = {'английский + русификатор по желанию', 'english (only!)', 'aнглийский', 'english +11',
                   'английский |multi8', 'английский |multi', 'английский + русификатор', 'english + русификаторы',
                   'английский', 'rus|eng|multi', 'english +7', 'английский |multi6',
                   'английский + русский (отдельной установкой)', 'english + русификатор'}
    french_set = {'французский', 'французски', 'французский + русификатор', 'français', 'french'}
    italian_set = {'итальянский', 'italiano', 'italiano 日本語'}
    japanese_set = {'японский', '日本語', 'японский + русский отдельно', 'японский (2)', 'японский + русификатор'}
    korean_set = {'корейский', '한국어', '한국어 + русификатор'}
    polish_set = {'polski', 'польский', 'pольский'}
    ukrainian_set = {'украинский', 'yкраїнська', 'українська'}
    czech_set = {'чешский +11', 'чешский', 'český'}
    norwegian_set = {'норвежский', 'norsk', 'svenska'}
    turkish_set = {'турецкий', 'türkçe', 'türkçe + русификатор'}
    hungarian_set = {'венгерский', 'magyar'}
    thai_set = {'тайский'}
    danish_set = {'датский', 'dansk'}
    indonesian_set = {'индонезийский'}
    romanian_set = {'румынский', 'română'}
    swedish_set = {'шведский'}
    dutch_set = {'нидерландский', 'nederlands'}
    slovak_set = {'словацкий'}
    catalan_set = {'каталанский'}

    by_name = {
        'arab': arab_set,
        'chinese': chinese_set,
        'spanish': spanish_set,
        'portuguese': portuguese_set,
        'german': german_set,
        'russian': russian_set,
        'english': english_set,
        'french': french_set,
        'italian': italian_set,
        'japanese': japanese_set,
        'korean': korean_set,
        'polish': polish_set,
        'ukrainian': ukrainian_set,
        'czech': czech_set,
        'norwegian': norwegian_set,
        'turkish': turkish_set,
        'hungarian': hungarian_set,
        'thai': thai_set,
        'danish': danish_set,
        'indonesian': indonesian_set,
        'romanian': romanian_set,
        'swedish': swedish_set,
        'dutch': dutch_set,
        'slovak': slovak_set,
        'catalan': catalan_set,
    }


class Types:
    codex_set = {'cценовый релиз от codex', 'сценовый релиз от codex'}
    skidrow_set = {'cценовый релиз от skidrow'}
    cpy_set = {'сценовый релиз от cpy'}
    plaza_set = {'сценовый релиз от plaza'}
    prophet_set = {'cценовый релиз от prophet'}
    xatab_set = {'repack от xatab'}
    gog_set = {'лицензия от gog.com', 'релиз от gog', 'релиз от gog.com', 'цифровая лицензия gog.com',
               'gog-rip + моды в портативной версии'}
    origin_set = {'origin-rip'}
    uplay_set = {'папка с игрой (uplay)', 'папка установленной игры (uplay-rip)'}
    steam_set = {'папка установленной игры (steam)', 'папка с игрой (steam)', 'папка с игрой',
                 'папка установленной игры', 'портативная версия в архиве (steam)'}
    license_set = {'лицензия + crack (reloaded)', 'лицензия (бука)', 'лицензия'}
    repack_set = {'repack', 'repack (лицензии)'}
    rip_set = {'неофициальный rip', 'egs-rip', 'steam-rip', 'gog-rip + моды в портативной версии'}
    folder_set = {'папка с игрой (microsoft store)', 'папка установленной игры (egs)', 'папка игры', 'папка с игрой.',
                  'папка с игрой'}
    archive_set = {'архив', 'портативная версия в архиве'}

    by_name = {
        'codex': codex_set,
        'skidrow': skidrow_set,
        'cpy': cpy_set,
        'plaza': plaza_set,
        'prophet': prophet_set,
        'xatab': xatab_set,
        'gog': gog_set,
        'origin': origin_set,
        'uplay': uplay_set,
        'steam': steam_set,
        'license': license_set,
        'repack': repack_set,
        'rip': rip_set,
        'folder': folder_set,
        'archive': archive_set,
    }


class Tablets:
    codex_set = {
        'вшита [anonymous]', 'вшита', 'вшита (cpy+codex)', 'codex + machine4578',
        'codex+steamless', 'codex emu + steamless', 'codex', 'codex emu + steamless exe',
        'codex|rune', 'codex (установлена по умолчанию)', 'codex (по умолчанию)',
        'codex emu (по умолчанию)', 'codex + christsnatcher', 'вшита (codex)'
    }

    empress_set = {
        'empress | goldberg', 'empress-mr.goldberg', 'empress + goldberg steam emu.',
        'empress|mr_goldberg'
    }

    goldberg_set = {
        'smart steam emu', 'goldberg steam emu. + steamless', 'mr_goldberg + coldclientloader от rat431',
        'mr_goldberg steam emu', 'goldberg + steamless', 'goldberg emu+sse',
        'goldberg emu', 'goldberg'
    }

    darksiders_set = {'darksiders'}

    thredm_set = {
        '3dm', 'вшита (3dm', 'вшита (3dm)', 'codex (по умолчанию)',
        'вшита (3dm)', 'ali213 + 3dm + lumaemu (в архиве)'
    }

    rune_set = {
        'rune emu + modified exe (от christsnatcher)', 'rune emu (по умолчанию)',
        'rune + steamless', 'rune', 'rune + steamless', 'rune (по умолчанию)',
        'rune emu'
    }

    skidrow_set = {
        'skidrow', 'вшита (skidrow crackfix v2 + codex)', 'присутствует (skidrow)'
    }

    reloaded_set = {
        'reloaded', 'вшита [reloaded]', 'вшита {reloaded}',
        'присутствует (reloaded)', 'присутствует {reloaded}',
        'ali213 emu. | reloaded emu'
    }

    fairlight_set = {
        'fairlight', 'fairlight & custom launcher', 'codex | fairlight'
    }

    tenoke_set = {
        'tenoke + steamless', 'tenoke'
    }

    origin_set = {
        'origin emulator: cpy'
    }

    gog_set = {
        'не требуется (drm free от gog)', 'не требуется (drm-free от gog)',
        'не требуется (gog)', 'drm-free'
    }

    other_set = {
        'delusional', 'blizzless', 'не требуется (drm free от gog)', 'нет',
        'присутствует', 'не требуется (drm-free )', 'присутствует {prophet}',
        'присутствует {codex}', 'онлайн-обход защиты от [0xdeadc0de]',
        'online-fix', 'использован steamless [atom0s]', 'merumeru',
        'польский', 'турецкий', 'английский', 'бр. португальский',
        'cdx)', 'theta.v 1.04', '+24', '+16', '+12', '+11', '+15'
    }

    by_name = {
        'codex': codex_set,
        'empress': empress_set,
        'goldberg': goldberg_set,
        'darksiders': darksiders_set,
        '3dm': thredm_set,
        'rune': rune_set,
        'skidrow': skidrow_set,
        'reloaded': reloaded_set,
        'fairlight': fairlight_set,
        'tenoke': tenoke_set,
        'origin': origin_set,
        'gog': gog_set,
        'none': other_set,
    }
