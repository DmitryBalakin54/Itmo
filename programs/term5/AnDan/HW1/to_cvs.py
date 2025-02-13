import data
from datetime import datetime


def title():
    res = 'title,release_date,views,downloads,'
    for genre in data.genres_set:
        res += 'genre_' + genre.replace(' ', '_') + ','
    for pub_type in data.Types.by_name.keys():
        res += 'publication_type_' + pub_type + ','
    for interface_language in data.TextLanguages.by_name.keys():
        res += 'interface_language_' + interface_language + ','
    for voice_language in data.VoiceLanguages.by_name.keys():
        res += 'voice_language_' + voice_language + ','
    for tablet in data.Tablets.by_name.keys():
        res += 'tablet_' + tablet + ','
    res += 'min_hdd_space_Mb'
    return res


def write_genres(ff, genres):
    for genre in data.genres_set:
        if genre in genres:
            ff.write('1,')
        else:
            ff.write('0,')


def write_type(ff, cur_type_):
    for t in data.Types.by_name.keys():
        if cur_type_ in data.Types.by_name[t]:
            ff.write('1,')
        else:
            ff.write('0,')


def write_interface_lang(ff, cur_interface_langs_):
    for l in data.TextLanguages.by_name.keys():
        text_lang_flag = False
        for t in data.TextLanguages.by_name[l]:
            if t in cur_interface_langs_:
                ff.write('1,')
                text_lang_flag = True
                break
        if not text_lang_flag:
            ff.write('0,')


def write_voice_lang(ff, cur_voice_langs_):
    for l in data.VoiceLanguages.by_name.keys():
        voice_lang_flag = False
        for t in data.VoiceLanguages.by_name[l]:
            if t in cur_voice_langs_:
                ff.write('1,')
                voice_lang_flag = True
                break
        if not voice_lang_flag:
            ff.write('0,')


def write_tablet(ff, tablet):
    tablet_flag = False
    for l in data.Tablets.by_name.keys():
        tablet_flag_contains = False
        for t in data.Tablets.by_name[l]:
            if t in tablet:
                ff.write('1,')
                tablet_flag = True
                tablet_flag_contains = True
                break

        if not tablet_flag and l == 'none' and not tablet_flag_contains:
            ff.write('1,')
            tablet_flag = True
            tablet_flag_contains = True

        if not tablet_flag_contains:
            ff.write('0,')


with open('xatab_rpg__data.csv', 'w', encoding='utf-8') as f:
    with open('raw_data.tsv', 'r', encoding='utf-8') as in_f:
        f.write(title())
        f.write('\n')

        flag = False
        for game in in_f:
            if flag:
                cur_data = [s.strip()[1:-1] for s in game.split('\t')]
                cur_title = cur_data[0]
                cur_date = cur_data[1]
                cur_views = cur_data[2]
                cur_downloads = cur_data[3]
                cur_genres = cur_data[4]
                cur_type = cur_data[5]
                cur_interface_langs = cur_data[6]
                cur_voice_langs = cur_data[7]
                cur_tablet = cur_data[8]
                cur_min_hdd = cur_data[9]

                if len(cur_downloads) == 0 or len(cur_min_hdd) == 0:
                    continue

                f.write(cur_title)
                f.write(',')

                date_obj = datetime.strptime(cur_date, '%d.%m.%Y')
                timestamp = int(date_obj.timestamp())
                f.write(str(timestamp))
                f.write(',')

                f.write(cur_views)
                f.write(',')

                f.write(cur_downloads)
                f.write(',')

                write_genres(f, cur_genres)

                write_type(f, cur_type)

                write_interface_lang(f, cur_interface_langs)

                write_voice_lang(f, cur_voice_langs)

                write_tablet(f, cur_tablet)

                f.write(cur_min_hdd)
                f.write('\n')
            else:
                flag = True
