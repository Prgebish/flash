# Task Plan: emacs-flash

## Goal
Создать пакет для Emacs, реализующий функциональность flash.nvim — быструю навигацию по коду с метками (labels).

## Phases

- [x] Phase 1: Исследование
  - [x] 1.1 Изучить исходники flash.nvim (state, labeler, highlight, jump)
  - [x] 1.2 Изучить avy и другие аналоги в Emacs
  - [x] 1.3 Сравнительный анализ, определить преимущества flash.nvim

- [x] Phase 2: Проектирование
  - [x] 2.1 Определить scope MVP (только jump, multi-window, autojump)
  - [x] 2.2 Спроектировать модульную структуру
  - [x] 2.3 Определить структуры данных (state, match)
  - [x] 2.4 Описать основной цикл (search → label → highlight → input)

- [x] Phase 3: Реализация MVP
  - [x] 3.1 emacs-flash-state.el — cl-defstruct для state и match
  - [x] 3.2 emacs-flash-search.el — поиск совпадений в видимых окнах
  - [x] 3.3 emacs-flash-highlight.el — overlays для backdrop, matches, labels
  - [x] 3.4 emacs-flash-label.el — назначение меток с пропуском конфликтных
  - [x] 3.5 emacs-flash-jump.el — прыжок к метке, переключение окон
  - [x] 3.6 emacs-flash.el — главный модуль, основной цикл, defcustom
  - [x] 3.7 Ручное тестирование базового функционала

- [x] Phase 4: Расширенные функции
  - [x] 4.1 emacs-flash-evil.el — интеграция с evil-mode
  - [x] 4.2 Rainbow labels (разноцветные метки)
  - [x] 4.3 Case-fold настройка (уже реализована в Phase 3)

- [ ] Phase 5: Тестирование и документация
  - [ ] 5.1 ERT тесты для основных функций
  - [ ] 5.2 README.md с примерами использования
  - [ ] 5.3 Подготовка к публикации в MELPA

- [ ] Phase 6: Расширенные режимы
  - [x] 6.1 Char motions (f/t/F/T) — улучшенные движения по символам с метками
    - Поиск символа на текущей строке (или multi-line опционально)
    - Метки для быстрого выбора при нескольких совпадениях
    - Повтор через `;` и `,`
    - Интеграция с evil-mode (замена стандартных f/t/F/T)
  - [x] 6.2 Search integration — flash во время `/` и `?` поиска
    - Автоматический показ меток при вводе паттерна поиска
    - Toggle через `C-;` в командном режиме
    - Интеграция с evil-ex-search и isearch
    - Нажатие на метку прыгает к совпадению и выходит из поиска
  - [x] 6.3 Label positioning — настройка позиции метки
    - `after` — метка после совпадения (default)
    - `before` — метка перед совпадением
    - `overlay` — метка поверх первого символа совпадения
    - `eol` — метка в конце строки
  - [x] 6.4 Jump position — позиция курсора после прыжка
    - `start` — в начало совпадения (default)
    - `end` — в конец совпадения

- [x] Phase 7: Дополнительные опции
  - [x] 7.1 Jumplist integration — сохранение в jumplist перед прыжком
  - [x] 7.2 Search history — добавление паттерна в историю поиска
  - [x] 7.3 Nohlsearch — очистка подсветки после прыжка
  - [x] 7.4 Continue last search — продолжение последнего поиска
  - [x] 7.5 Min pattern length — минимальная длина для показа меток

- [ ] Phase 8: Расширенные режимы навигации
  - [ ] 8.1 Multi-char labels — двухсимвольные метки когда совпадений > 26
    - Первое нажатие фильтрует метки, второе — прыгает
    - Приоритет: высокий
  - [ ] 8.2 Line mode — прыжок к началу строк
    - Показывать метки на каждой видимой строке
    - Полезно для быстрой навигации по структуре
    - Приоритет: средний
  - [ ] 8.3 Word mode — прыжок по словам
    - Метки на начале каждого слова (как avy-goto-word)
    - Приоритет: низкий (evil w/b уже работают)
  - [ ] 8.4 Remote mode — операции без перемещения курсора
    - yr/dr/cr — yank/delete/change remote
    - После операции курсор остаётся на месте
    - Приоритет: низкий (workaround: jump + C-o)
  - [ ] 8.5 Treesitter mode — прыжок к узлам treesitter
    - Функции, классы, блоки, аргументы
    - Требует treesitter-интеграцию
    - Приоритет: низкий

## Blocked / Open Questions
(все решены)

## Decisions Made
- [Режим]: evil-mode — приоритет интеграции с evil
- [MVP scope]: Только базовый jump с инкрементальным вводом и метками
- [Multi-window]: Да, поддержка всех видимых окон с самого начала
- [Overlays]: Использовать overlays для всего отображения (не text-properties)
- [Требования]: Emacs >= 27.1 (cl-defstruct, when-let)
- [Prompt]: Использовать minibuffer (не отдельный buffer)
- [Folded regions]: Как в flash.nvim — искать внутри folds, показывать одну метку на fold, разворачивать при прыжке
- [Тесты]: ERT тесты для каждого модуля, все тесты должны проходить

## Status
**Phase 7 завершена** — 72 теста проходят

**Примеры тестирования:** examples.md обновлён (сценарии 19-27)

**Следующий этап:** Phase 5 (документация) и git commit

### Новые возможности:

**Phase 7:**
- Jumplist integration — `emacs-flash-jumplist` (default: t)
  - Сохраняет позицию в mark ring перед прыжком
  - Возврат через `C-u C-SPC` или evil `C-o`
- Search history — `emacs-flash-search-history` (default: nil)
  - Добавляет паттерн в историю isearch
- Nohlsearch — `emacs-flash-nohlsearch` (default: nil)
  - Очищает подсветку поиска после прыжка
- Continue last search — `emacs-flash-jump-continue`
  - Команда для продолжения последнего поиска
- Min pattern length — `emacs-flash-min-pattern-length` (default: 0)
  - Минимальная длина паттерна для показа меток

**Phase 6.4:**
- Jump position — позиция курсора после прыжка
- `emacs-flash-jump-position` — defcustom с 2 опциями:
  - `start` — в начало совпадения (default)
  - `end` — в конец совпадения

**Phase 6.3:**
- Label positioning — настройка позиции метки
- `emacs-flash-label-position` — defcustom с 4 опциями:
  - `after` — после совпадения (default, как flash.nvim)
  - `before` — перед совпадением
  - `overlay` — поверх первого символа
  - `eol` — в конце строки

**Phase 6.2:**
- Search integration — flash метки во время `/` и `?` поиска
- `emacs-flash-isearch-mode` — глобальный minor mode
- Поддержка evil-ex-search и стандартного isearch
- Toggle через `C-;` во время поиска (использует emulation-mode-map-alists для приоритета над evil)
- Нажатие на метку прыгает и выходит из поиска
- **Trigger mechanism**: `emacs-flash-isearch-trigger` (default: nil)
  - Опциональный режим: установить `";"` для активации через триггер
  - При активации: нажать `;` → `[label?]` → нажать метку для прыжка
- **Smart skip**: метки, которые могут продолжить поисковый паттерн, автоматически пропускаются
  - Поиск конфликтов происходит по всему буферу (как в flash.nvim)

**Phase 6.1:**
- Char motions (f/t/F/T) — улучшенные движения по символам
- Мгновенный прыжок к первому совпадению + метки для остальных
- Повтор через `;` и `,`
- `emacs-flash-char-jump-labels` — включить/выключить метки
- `emacs-flash-char-multi-line` — поиск за пределами строки
- Интеграция с evil-mode через `emacs-flash-char-setup-evil-keys`

**Phase 4.2:**
- Rainbow labels с поддержкой светлых/тёмных тем
- `emacs-flash-highlight-matches` — опция отключения подсветки совпадений
- Faces адаптируются к теме через наследование от font-lock

**Phase 4.3:**
- ~~Regex search~~ (удалено — держим простым, literal search достаточен для навигации)
- Case-fold уже был реализован в Phase 3 (`emacs-flash-case-fold`)

## Files
- `task_plan.md` — этот файл
- `architecture.md` — детальная архитектура пакета
- `examples.md` — сценарии тестирования всех фич
- `CLAUDE.md` — инструкции + error log + решения проблем
