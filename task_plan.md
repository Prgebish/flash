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
  - [ ] 6.3 Label positioning — настройка позиции метки
    - before — метка перед совпадением
    - after — метка после совпадения (текущее поведение)
    - overlay — метка поверх первого символа совпадения
    - inline — метка внутри текста (смещает текст)

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
**Phase 6.2 завершена** — 77 тестов проходят

**Следующий этап:** Phase 5 (документация) или Phase 6.3 (label positioning)

### Новые возможности:

**Phase 6.2:**
- Search integration — flash метки во время `/` и `?` поиска
- `emacs-flash-isearch-mode` — глобальный minor mode
- Поддержка evil-ex-search и стандартного isearch
- Toggle через `C-;` во время поиска
- Нажатие на метку прыгает и выходит из поиска

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
